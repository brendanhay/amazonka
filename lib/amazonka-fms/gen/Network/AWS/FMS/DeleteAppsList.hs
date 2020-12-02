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
-- Module      : Network.AWS.FMS.DeleteAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager applications list.
module Network.AWS.FMS.DeleteAppsList
  ( -- * Creating a Request
    deleteAppsList,
    DeleteAppsList,

    -- * Request Lenses
    dalListId,

    -- * Destructuring the Response
    deleteAppsListResponse,
    DeleteAppsListResponse,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAppsList' smart constructor.
newtype DeleteAppsList = DeleteAppsList' {_dalListId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalListId' - The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
deleteAppsList ::
  -- | 'dalListId'
  Text ->
  DeleteAppsList
deleteAppsList pListId_ = DeleteAppsList' {_dalListId = pListId_}

-- | The ID of the applications list that you want to delete. You can retrieve this ID from @PutAppsList@ , @ListAppsLists@ , and @GetAppsList@ .
dalListId :: Lens' DeleteAppsList Text
dalListId = lens _dalListId (\s a -> s {_dalListId = a})

instance AWSRequest DeleteAppsList where
  type Rs DeleteAppsList = DeleteAppsListResponse
  request = postJSON fms
  response = receiveNull DeleteAppsListResponse'

instance Hashable DeleteAppsList

instance NFData DeleteAppsList

instance ToHeaders DeleteAppsList where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSFMS_20180101.DeleteAppsList" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAppsList where
  toJSON DeleteAppsList' {..} =
    object (catMaybes [Just ("ListId" .= _dalListId)])

instance ToPath DeleteAppsList where
  toPath = const "/"

instance ToQuery DeleteAppsList where
  toQuery = const mempty

-- | /See:/ 'deleteAppsListResponse' smart constructor.
data DeleteAppsListResponse = DeleteAppsListResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppsListResponse' with the minimum fields required to make a request.
deleteAppsListResponse ::
  DeleteAppsListResponse
deleteAppsListResponse = DeleteAppsListResponse'

instance NFData DeleteAppsListResponse
