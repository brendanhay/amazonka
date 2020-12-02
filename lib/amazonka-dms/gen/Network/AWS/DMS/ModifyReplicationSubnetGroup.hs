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
-- Module      : Network.AWS.DMS.ModifyReplicationSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for the specified replication subnet group.
--
--
module Network.AWS.DMS.ModifyReplicationSubnetGroup
    (
    -- * Creating a Request
      modifyReplicationSubnetGroup
    , ModifyReplicationSubnetGroup
    -- * Request Lenses
    , mrsgReplicationSubnetGroupDescription
    , mrsgReplicationSubnetGroupIdentifier
    , mrsgSubnetIds

    -- * Destructuring the Response
    , modifyReplicationSubnetGroupResponse
    , ModifyReplicationSubnetGroupResponse
    -- * Response Lenses
    , mrsgrsReplicationSubnetGroup
    , mrsgrsResponseStatus
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
-- /See:/ 'modifyReplicationSubnetGroup' smart constructor.
data ModifyReplicationSubnetGroup = ModifyReplicationSubnetGroup'
  { _mrsgReplicationSubnetGroupDescription :: !(Maybe Text)
  , _mrsgReplicationSubnetGroupIdentifier  :: !Text
  , _mrsgSubnetIds                         :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrsgReplicationSubnetGroupDescription' - The description of the replication instance subnet group.
--
-- * 'mrsgReplicationSubnetGroupIdentifier' - The name of the replication instance subnet group.
--
-- * 'mrsgSubnetIds' - A list of subnet IDs.
modifyReplicationSubnetGroup
    :: Text -- ^ 'mrsgReplicationSubnetGroupIdentifier'
    -> ModifyReplicationSubnetGroup
modifyReplicationSubnetGroup pReplicationSubnetGroupIdentifier_ =
  ModifyReplicationSubnetGroup'
    { _mrsgReplicationSubnetGroupDescription = Nothing
    , _mrsgReplicationSubnetGroupIdentifier = pReplicationSubnetGroupIdentifier_
    , _mrsgSubnetIds = mempty
    }


-- | The description of the replication instance subnet group.
mrsgReplicationSubnetGroupDescription :: Lens' ModifyReplicationSubnetGroup (Maybe Text)
mrsgReplicationSubnetGroupDescription = lens _mrsgReplicationSubnetGroupDescription (\ s a -> s{_mrsgReplicationSubnetGroupDescription = a})

-- | The name of the replication instance subnet group.
mrsgReplicationSubnetGroupIdentifier :: Lens' ModifyReplicationSubnetGroup Text
mrsgReplicationSubnetGroupIdentifier = lens _mrsgReplicationSubnetGroupIdentifier (\ s a -> s{_mrsgReplicationSubnetGroupIdentifier = a})

-- | A list of subnet IDs.
mrsgSubnetIds :: Lens' ModifyReplicationSubnetGroup [Text]
mrsgSubnetIds = lens _mrsgSubnetIds (\ s a -> s{_mrsgSubnetIds = a}) . _Coerce

instance AWSRequest ModifyReplicationSubnetGroup
         where
        type Rs ModifyReplicationSubnetGroup =
             ModifyReplicationSubnetGroupResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ModifyReplicationSubnetGroupResponse' <$>
                   (x .?> "ReplicationSubnetGroup") <*>
                     (pure (fromEnum s)))

instance Hashable ModifyReplicationSubnetGroup where

instance NFData ModifyReplicationSubnetGroup where

instance ToHeaders ModifyReplicationSubnetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ModifyReplicationSubnetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyReplicationSubnetGroup where
        toJSON ModifyReplicationSubnetGroup'{..}
          = object
              (catMaybes
                 [("ReplicationSubnetGroupDescription" .=) <$>
                    _mrsgReplicationSubnetGroupDescription,
                  Just
                    ("ReplicationSubnetGroupIdentifier" .=
                       _mrsgReplicationSubnetGroupIdentifier),
                  Just ("SubnetIds" .= _mrsgSubnetIds)])

instance ToPath ModifyReplicationSubnetGroup where
        toPath = const "/"

instance ToQuery ModifyReplicationSubnetGroup where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'modifyReplicationSubnetGroupResponse' smart constructor.
data ModifyReplicationSubnetGroupResponse = ModifyReplicationSubnetGroupResponse'
  { _mrsgrsReplicationSubnetGroup :: !(Maybe ReplicationSubnetGroup)
  , _mrsgrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrsgrsReplicationSubnetGroup' - The modified replication subnet group.
--
-- * 'mrsgrsResponseStatus' - -- | The response status code.
modifyReplicationSubnetGroupResponse
    :: Int -- ^ 'mrsgrsResponseStatus'
    -> ModifyReplicationSubnetGroupResponse
modifyReplicationSubnetGroupResponse pResponseStatus_ =
  ModifyReplicationSubnetGroupResponse'
    { _mrsgrsReplicationSubnetGroup = Nothing
    , _mrsgrsResponseStatus = pResponseStatus_
    }


-- | The modified replication subnet group.
mrsgrsReplicationSubnetGroup :: Lens' ModifyReplicationSubnetGroupResponse (Maybe ReplicationSubnetGroup)
mrsgrsReplicationSubnetGroup = lens _mrsgrsReplicationSubnetGroup (\ s a -> s{_mrsgrsReplicationSubnetGroup = a})

-- | -- | The response status code.
mrsgrsResponseStatus :: Lens' ModifyReplicationSubnetGroupResponse Int
mrsgrsResponseStatus = lens _mrsgrsResponseStatus (\ s a -> s{_mrsgrsResponseStatus = a})

instance NFData ModifyReplicationSubnetGroupResponse
         where
