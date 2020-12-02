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
-- Module      : Network.AWS.CloudFront.DeleteKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key group.
--
--
-- You cannot delete a key group that is referenced in a cache behavior. First update your distributions to remove the key group from all cache behaviors, then delete the key group.
--
-- To delete a key group, you must provide the key group’s identifier and version. To get these values, use @ListKeyGroups@ followed by @GetKeyGroup@ or @GetKeyGroupConfig@ .
module Network.AWS.CloudFront.DeleteKeyGroup
  ( -- * Creating a Request
    deleteKeyGroup,
    DeleteKeyGroup,

    -- * Request Lenses
    dkgIfMatch,
    dkgId,

    -- * Destructuring the Response
    deleteKeyGroupResponse,
    DeleteKeyGroupResponse,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteKeyGroup' smart constructor.
data DeleteKeyGroup = DeleteKeyGroup'
  { _dkgIfMatch :: !(Maybe Text),
    _dkgId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteKeyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkgIfMatch' - The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
--
-- * 'dkgId' - The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
deleteKeyGroup ::
  -- | 'dkgId'
  Text ->
  DeleteKeyGroup
deleteKeyGroup pId_ =
  DeleteKeyGroup' {_dkgIfMatch = Nothing, _dkgId = pId_}

-- | The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
dkgIfMatch :: Lens' DeleteKeyGroup (Maybe Text)
dkgIfMatch = lens _dkgIfMatch (\s a -> s {_dkgIfMatch = a})

-- | The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
dkgId :: Lens' DeleteKeyGroup Text
dkgId = lens _dkgId (\s a -> s {_dkgId = a})

instance AWSRequest DeleteKeyGroup where
  type Rs DeleteKeyGroup = DeleteKeyGroupResponse
  request = delete cloudFront
  response = receiveNull DeleteKeyGroupResponse'

instance Hashable DeleteKeyGroup

instance NFData DeleteKeyGroup

instance ToHeaders DeleteKeyGroup where
  toHeaders DeleteKeyGroup' {..} = mconcat ["If-Match" =# _dkgIfMatch]

instance ToPath DeleteKeyGroup where
  toPath DeleteKeyGroup' {..} =
    mconcat ["/2020-05-31/key-group/", toBS _dkgId]

instance ToQuery DeleteKeyGroup where
  toQuery = const mempty

-- | /See:/ 'deleteKeyGroupResponse' smart constructor.
data DeleteKeyGroupResponse = DeleteKeyGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteKeyGroupResponse' with the minimum fields required to make a request.
deleteKeyGroupResponse ::
  DeleteKeyGroupResponse
deleteKeyGroupResponse = DeleteKeyGroupResponse'

instance NFData DeleteKeyGroupResponse
