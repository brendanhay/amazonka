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
-- Module      : Network.AWS.RDS.DeleteGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a global database cluster. The primary and secondary clusters must already be detached or destroyed first.
module Network.AWS.RDS.DeleteGlobalCluster
  ( -- * Creating a Request
    deleteGlobalCluster,
    DeleteGlobalCluster,

    -- * Request Lenses
    dgcGlobalClusterIdentifier,

    -- * Destructuring the Response
    deleteGlobalClusterResponse,
    DeleteGlobalClusterResponse,

    -- * Response Lenses
    dgcrsGlobalCluster,
    dgcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGlobalCluster' smart constructor.
newtype DeleteGlobalCluster = DeleteGlobalCluster'
  { _dgcGlobalClusterIdentifier ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGlobalCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgcGlobalClusterIdentifier' - The cluster identifier of the global database cluster being deleted.
deleteGlobalCluster ::
  -- | 'dgcGlobalClusterIdentifier'
  Text ->
  DeleteGlobalCluster
deleteGlobalCluster pGlobalClusterIdentifier_ =
  DeleteGlobalCluster'
    { _dgcGlobalClusterIdentifier =
        pGlobalClusterIdentifier_
    }

-- | The cluster identifier of the global database cluster being deleted.
dgcGlobalClusterIdentifier :: Lens' DeleteGlobalCluster Text
dgcGlobalClusterIdentifier = lens _dgcGlobalClusterIdentifier (\s a -> s {_dgcGlobalClusterIdentifier = a})

instance AWSRequest DeleteGlobalCluster where
  type Rs DeleteGlobalCluster = DeleteGlobalClusterResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DeleteGlobalClusterResult"
      ( \s h x ->
          DeleteGlobalClusterResponse'
            <$> (x .@? "GlobalCluster") <*> (pure (fromEnum s))
      )

instance Hashable DeleteGlobalCluster

instance NFData DeleteGlobalCluster

instance ToHeaders DeleteGlobalCluster where
  toHeaders = const mempty

instance ToPath DeleteGlobalCluster where
  toPath = const "/"

instance ToQuery DeleteGlobalCluster where
  toQuery DeleteGlobalCluster' {..} =
    mconcat
      [ "Action" =: ("DeleteGlobalCluster" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "GlobalClusterIdentifier" =: _dgcGlobalClusterIdentifier
      ]

-- | /See:/ 'deleteGlobalClusterResponse' smart constructor.
data DeleteGlobalClusterResponse = DeleteGlobalClusterResponse'
  { _dgcrsGlobalCluster ::
      !(Maybe GlobalCluster),
    _dgcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGlobalClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgcrsGlobalCluster' - Undocumented member.
--
-- * 'dgcrsResponseStatus' - -- | The response status code.
deleteGlobalClusterResponse ::
  -- | 'dgcrsResponseStatus'
  Int ->
  DeleteGlobalClusterResponse
deleteGlobalClusterResponse pResponseStatus_ =
  DeleteGlobalClusterResponse'
    { _dgcrsGlobalCluster = Nothing,
      _dgcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dgcrsGlobalCluster :: Lens' DeleteGlobalClusterResponse (Maybe GlobalCluster)
dgcrsGlobalCluster = lens _dgcrsGlobalCluster (\s a -> s {_dgcrsGlobalCluster = a})

-- | -- | The response status code.
dgcrsResponseStatus :: Lens' DeleteGlobalClusterResponse Int
dgcrsResponseStatus = lens _dgcrsResponseStatus (\s a -> s {_dgcrsResponseStatus = a})

instance NFData DeleteGlobalClusterResponse
