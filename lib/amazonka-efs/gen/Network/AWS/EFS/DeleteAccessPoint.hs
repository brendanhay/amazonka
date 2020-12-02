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
-- Module      : Network.AWS.EFS.DeleteAccessPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified access point. After deletion is complete, new clients can no longer connect to the access points. Clients connected to the access point at the time of deletion will continue to function until they terminate their connection.
--
--
-- This operation requires permissions for the @elasticfilesystem:DeleteAccessPoint@ action.
module Network.AWS.EFS.DeleteAccessPoint
  ( -- * Creating a Request
    deleteAccessPoint,
    DeleteAccessPoint,

    -- * Request Lenses
    dAccessPointId,

    -- * Destructuring the Response
    deleteAccessPointResponse,
    DeleteAccessPointResponse,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccessPoint' smart constructor.
newtype DeleteAccessPoint = DeleteAccessPoint'
  { _dAccessPointId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccessPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAccessPointId' - The ID of the access point that you want to delete.
deleteAccessPoint ::
  -- | 'dAccessPointId'
  Text ->
  DeleteAccessPoint
deleteAccessPoint pAccessPointId_ =
  DeleteAccessPoint' {_dAccessPointId = pAccessPointId_}

-- | The ID of the access point that you want to delete.
dAccessPointId :: Lens' DeleteAccessPoint Text
dAccessPointId = lens _dAccessPointId (\s a -> s {_dAccessPointId = a})

instance AWSRequest DeleteAccessPoint where
  type Rs DeleteAccessPoint = DeleteAccessPointResponse
  request = delete efs
  response = receiveNull DeleteAccessPointResponse'

instance Hashable DeleteAccessPoint

instance NFData DeleteAccessPoint

instance ToHeaders DeleteAccessPoint where
  toHeaders = const mempty

instance ToPath DeleteAccessPoint where
  toPath DeleteAccessPoint' {..} =
    mconcat ["/2015-02-01/access-points/", toBS _dAccessPointId]

instance ToQuery DeleteAccessPoint where
  toQuery = const mempty

-- | /See:/ 'deleteAccessPointResponse' smart constructor.
data DeleteAccessPointResponse = DeleteAccessPointResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccessPointResponse' with the minimum fields required to make a request.
deleteAccessPointResponse ::
  DeleteAccessPointResponse
deleteAccessPointResponse = DeleteAccessPointResponse'

instance NFData DeleteAccessPointResponse
