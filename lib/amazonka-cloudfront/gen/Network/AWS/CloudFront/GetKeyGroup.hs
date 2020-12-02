{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group, including the date and time when the key group was last modified.
--
--
-- To get a key group, you must provide the key group’s identifier. If the key group is referenced in a distribution’s cache behavior, you can get the key group’s identifier using @ListDistributions@ or @GetDistribution@ . If the key group is not referenced in a cache behavior, you can get the identifier using @ListKeyGroups@ .
module Network.AWS.CloudFront.GetKeyGroup
  ( -- * Creating a Request
    getKeyGroup,
    GetKeyGroup,

    -- * Request Lenses
    gkgId,

    -- * Destructuring the Response
    getKeyGroupResponse,
    GetKeyGroupResponse,

    -- * Response Lenses
    gkgrsETag,
    gkgrsKeyGroup,
    gkgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyGroup' smart constructor.
newtype GetKeyGroup = GetKeyGroup' {_gkgId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkgId' - The identifier of the key group that you are getting. To get the identifier, use @ListKeyGroups@ .
getKeyGroup ::
  -- | 'gkgId'
  Text ->
  GetKeyGroup
getKeyGroup pId_ = GetKeyGroup' {_gkgId = pId_}

-- | The identifier of the key group that you are getting. To get the identifier, use @ListKeyGroups@ .
gkgId :: Lens' GetKeyGroup Text
gkgId = lens _gkgId (\s a -> s {_gkgId = a})

instance AWSRequest GetKeyGroup where
  type Rs GetKeyGroup = GetKeyGroupResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetKeyGroupResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetKeyGroup

instance NFData GetKeyGroup

instance ToHeaders GetKeyGroup where
  toHeaders = const mempty

instance ToPath GetKeyGroup where
  toPath GetKeyGroup' {..} =
    mconcat ["/2020-05-31/key-group/", toBS _gkgId]

instance ToQuery GetKeyGroup where
  toQuery = const mempty

-- | /See:/ 'getKeyGroupResponse' smart constructor.
data GetKeyGroupResponse = GetKeyGroupResponse'
  { _gkgrsETag ::
      !(Maybe Text),
    _gkgrsKeyGroup :: !(Maybe KeyGroup),
    _gkgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkgrsETag' - The identifier for this version of the key group.
--
-- * 'gkgrsKeyGroup' - The key group.
--
-- * 'gkgrsResponseStatus' - -- | The response status code.
getKeyGroupResponse ::
  -- | 'gkgrsResponseStatus'
  Int ->
  GetKeyGroupResponse
getKeyGroupResponse pResponseStatus_ =
  GetKeyGroupResponse'
    { _gkgrsETag = Nothing,
      _gkgrsKeyGroup = Nothing,
      _gkgrsResponseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
gkgrsETag :: Lens' GetKeyGroupResponse (Maybe Text)
gkgrsETag = lens _gkgrsETag (\s a -> s {_gkgrsETag = a})

-- | The key group.
gkgrsKeyGroup :: Lens' GetKeyGroupResponse (Maybe KeyGroup)
gkgrsKeyGroup = lens _gkgrsKeyGroup (\s a -> s {_gkgrsKeyGroup = a})

-- | -- | The response status code.
gkgrsResponseStatus :: Lens' GetKeyGroupResponse Int
gkgrsResponseStatus = lens _gkgrsResponseStatus (\s a -> s {_gkgrsResponseStatus = a})

instance NFData GetKeyGroupResponse
