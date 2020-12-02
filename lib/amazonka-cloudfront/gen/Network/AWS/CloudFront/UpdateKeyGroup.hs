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
-- Module      : Network.AWS.CloudFront.UpdateKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a key group.
--
--
-- When you update a key group, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update a key group:
--
--     * Get the current key group with @GetKeyGroup@ or @GetKeyGroupConfig@ .
--
--     * Locally modify the fields in the key group that you want to update. For example, add or remove public key IDs.
--
--     * Call @UpdateKeyGroup@ with the entire key group object, including the fields that you modified and those that you didn’t.
module Network.AWS.CloudFront.UpdateKeyGroup
  ( -- * Creating a Request
    updateKeyGroup,
    UpdateKeyGroup,

    -- * Request Lenses
    ukgIfMatch,
    ukgKeyGroupConfig,
    ukgId,

    -- * Destructuring the Response
    updateKeyGroupResponse,
    UpdateKeyGroupResponse,

    -- * Response Lenses
    ukgrsETag,
    ukgrsKeyGroup,
    ukgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateKeyGroup' smart constructor.
data UpdateKeyGroup = UpdateKeyGroup'
  { _ukgIfMatch :: !(Maybe Text),
    _ukgKeyGroupConfig :: !KeyGroupConfig,
    _ukgId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateKeyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ukgIfMatch' - The version of the key group that you are updating. The version is the key group’s @ETag@ value.
--
-- * 'ukgKeyGroupConfig' - The key group configuration.
--
-- * 'ukgId' - The identifier of the key group that you are updating.
updateKeyGroup ::
  -- | 'ukgKeyGroupConfig'
  KeyGroupConfig ->
  -- | 'ukgId'
  Text ->
  UpdateKeyGroup
updateKeyGroup pKeyGroupConfig_ pId_ =
  UpdateKeyGroup'
    { _ukgIfMatch = Nothing,
      _ukgKeyGroupConfig = pKeyGroupConfig_,
      _ukgId = pId_
    }

-- | The version of the key group that you are updating. The version is the key group’s @ETag@ value.
ukgIfMatch :: Lens' UpdateKeyGroup (Maybe Text)
ukgIfMatch = lens _ukgIfMatch (\s a -> s {_ukgIfMatch = a})

-- | The key group configuration.
ukgKeyGroupConfig :: Lens' UpdateKeyGroup KeyGroupConfig
ukgKeyGroupConfig = lens _ukgKeyGroupConfig (\s a -> s {_ukgKeyGroupConfig = a})

-- | The identifier of the key group that you are updating.
ukgId :: Lens' UpdateKeyGroup Text
ukgId = lens _ukgId (\s a -> s {_ukgId = a})

instance AWSRequest UpdateKeyGroup where
  type Rs UpdateKeyGroup = UpdateKeyGroupResponse
  request = putXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          UpdateKeyGroupResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable UpdateKeyGroup

instance NFData UpdateKeyGroup

instance ToElement UpdateKeyGroup where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      . _ukgKeyGroupConfig

instance ToHeaders UpdateKeyGroup where
  toHeaders UpdateKeyGroup' {..} = mconcat ["If-Match" =# _ukgIfMatch]

instance ToPath UpdateKeyGroup where
  toPath UpdateKeyGroup' {..} =
    mconcat ["/2020-05-31/key-group/", toBS _ukgId]

instance ToQuery UpdateKeyGroup where
  toQuery = const mempty

-- | /See:/ 'updateKeyGroupResponse' smart constructor.
data UpdateKeyGroupResponse = UpdateKeyGroupResponse'
  { _ukgrsETag ::
      !(Maybe Text),
    _ukgrsKeyGroup :: !(Maybe KeyGroup),
    _ukgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateKeyGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ukgrsETag' - The identifier for this version of the key group.
--
-- * 'ukgrsKeyGroup' - The key group that was just updated.
--
-- * 'ukgrsResponseStatus' - -- | The response status code.
updateKeyGroupResponse ::
  -- | 'ukgrsResponseStatus'
  Int ->
  UpdateKeyGroupResponse
updateKeyGroupResponse pResponseStatus_ =
  UpdateKeyGroupResponse'
    { _ukgrsETag = Nothing,
      _ukgrsKeyGroup = Nothing,
      _ukgrsResponseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
ukgrsETag :: Lens' UpdateKeyGroupResponse (Maybe Text)
ukgrsETag = lens _ukgrsETag (\s a -> s {_ukgrsETag = a})

-- | The key group that was just updated.
ukgrsKeyGroup :: Lens' UpdateKeyGroupResponse (Maybe KeyGroup)
ukgrsKeyGroup = lens _ukgrsKeyGroup (\s a -> s {_ukgrsKeyGroup = a})

-- | -- | The response status code.
ukgrsResponseStatus :: Lens' UpdateKeyGroupResponse Int
ukgrsResponseStatus = lens _ukgrsResponseStatus (\s a -> s {_ukgrsResponseStatus = a})

instance NFData UpdateKeyGroupResponse
