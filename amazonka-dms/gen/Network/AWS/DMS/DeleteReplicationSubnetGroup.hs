{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
--
--
module Network.AWS.DMS.DeleteReplicationSubnetGroup
    (
    -- * Creating a Request
      deleteReplicationSubnetGroup
    , DeleteReplicationSubnetGroup
    -- * Request Lenses
    , drsgReplicationSubnetGroupIdentifier

    -- * Destructuring the Response
    , deleteReplicationSubnetGroupResponse
    , DeleteReplicationSubnetGroupResponse
    -- * Response Lenses
    , drsgrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteReplicationSubnetGroup' smart constructor.
newtype DeleteReplicationSubnetGroup = DeleteReplicationSubnetGroup'
  { _drsgReplicationSubnetGroupIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsgReplicationSubnetGroupIdentifier' - The subnet group name of the replication instance.
deleteReplicationSubnetGroup
    :: Text -- ^ 'drsgReplicationSubnetGroupIdentifier'
    -> DeleteReplicationSubnetGroup
deleteReplicationSubnetGroup pReplicationSubnetGroupIdentifier_ =
  DeleteReplicationSubnetGroup'
    {_drsgReplicationSubnetGroupIdentifier = pReplicationSubnetGroupIdentifier_}


-- | The subnet group name of the replication instance.
drsgReplicationSubnetGroupIdentifier :: Lens' DeleteReplicationSubnetGroup Text
drsgReplicationSubnetGroupIdentifier = lens _drsgReplicationSubnetGroupIdentifier (\ s a -> s{_drsgReplicationSubnetGroupIdentifier = a})

instance AWSRequest DeleteReplicationSubnetGroup
         where
        type Rs DeleteReplicationSubnetGroup =
             DeleteReplicationSubnetGroupResponse
        request = postJSON dms
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteReplicationSubnetGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteReplicationSubnetGroup where

instance NFData DeleteReplicationSubnetGroup where

instance ToHeaders DeleteReplicationSubnetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DeleteReplicationSubnetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteReplicationSubnetGroup where
        toJSON DeleteReplicationSubnetGroup'{..}
          = object
              (catMaybes
                 [Just
                    ("ReplicationSubnetGroupIdentifier" .=
                       _drsgReplicationSubnetGroupIdentifier)])

instance ToPath DeleteReplicationSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteReplicationSubnetGroup where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'deleteReplicationSubnetGroupResponse' smart constructor.
newtype DeleteReplicationSubnetGroupResponse = DeleteReplicationSubnetGroupResponse'
  { _drsgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReplicationSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsgrsResponseStatus' - -- | The response status code.
deleteReplicationSubnetGroupResponse
    :: Int -- ^ 'drsgrsResponseStatus'
    -> DeleteReplicationSubnetGroupResponse
deleteReplicationSubnetGroupResponse pResponseStatus_ =
  DeleteReplicationSubnetGroupResponse'
    {_drsgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsgrsResponseStatus :: Lens' DeleteReplicationSubnetGroupResponse Int
drsgrsResponseStatus = lens _drsgrsResponseStatus (\ s a -> s{_drsgrsResponseStatus = a})

instance NFData DeleteReplicationSubnetGroupResponse
         where
