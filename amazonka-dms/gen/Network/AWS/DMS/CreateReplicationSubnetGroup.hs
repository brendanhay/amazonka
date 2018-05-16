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
-- Module      : Network.AWS.DMS.CreateReplicationSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication subnet group given a list of the subnet IDs in a VPC.
--
--
module Network.AWS.DMS.CreateReplicationSubnetGroup
    (
    -- * Creating a Request
      createReplicationSubnetGroup
    , CreateReplicationSubnetGroup
    -- * Request Lenses
    , crsgTags
    , crsgReplicationSubnetGroupIdentifier
    , crsgReplicationSubnetGroupDescription
    , crsgSubnetIds

    -- * Destructuring the Response
    , createReplicationSubnetGroupResponse
    , CreateReplicationSubnetGroupResponse
    -- * Response Lenses
    , crsgrsReplicationSubnetGroup
    , crsgrsResponseStatus
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
-- /See:/ 'createReplicationSubnetGroup' smart constructor.
data CreateReplicationSubnetGroup = CreateReplicationSubnetGroup'
  { _crsgTags                              :: !(Maybe [Tag])
  , _crsgReplicationSubnetGroupIdentifier  :: !Text
  , _crsgReplicationSubnetGroupDescription :: !Text
  , _crsgSubnetIds                         :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsgTags' - The tag to be assigned to the subnet group.
--
-- * 'crsgReplicationSubnetGroupIdentifier' - The name for the replication subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default". Example: @mySubnetgroup@
--
-- * 'crsgReplicationSubnetGroupDescription' - The description for the subnet group.
--
-- * 'crsgSubnetIds' - The EC2 subnet IDs for the subnet group.
createReplicationSubnetGroup
    :: Text -- ^ 'crsgReplicationSubnetGroupIdentifier'
    -> Text -- ^ 'crsgReplicationSubnetGroupDescription'
    -> CreateReplicationSubnetGroup
createReplicationSubnetGroup pReplicationSubnetGroupIdentifier_ pReplicationSubnetGroupDescription_ =
  CreateReplicationSubnetGroup'
    { _crsgTags = Nothing
    , _crsgReplicationSubnetGroupIdentifier = pReplicationSubnetGroupIdentifier_
    , _crsgReplicationSubnetGroupDescription =
        pReplicationSubnetGroupDescription_
    , _crsgSubnetIds = mempty
    }


-- | The tag to be assigned to the subnet group.
crsgTags :: Lens' CreateReplicationSubnetGroup [Tag]
crsgTags = lens _crsgTags (\ s a -> s{_crsgTags = a}) . _Default . _Coerce

-- | The name for the replication subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 alphanumeric characters, periods, spaces, underscores, or hyphens. Must not be "default". Example: @mySubnetgroup@
crsgReplicationSubnetGroupIdentifier :: Lens' CreateReplicationSubnetGroup Text
crsgReplicationSubnetGroupIdentifier = lens _crsgReplicationSubnetGroupIdentifier (\ s a -> s{_crsgReplicationSubnetGroupIdentifier = a})

-- | The description for the subnet group.
crsgReplicationSubnetGroupDescription :: Lens' CreateReplicationSubnetGroup Text
crsgReplicationSubnetGroupDescription = lens _crsgReplicationSubnetGroupDescription (\ s a -> s{_crsgReplicationSubnetGroupDescription = a})

-- | The EC2 subnet IDs for the subnet group.
crsgSubnetIds :: Lens' CreateReplicationSubnetGroup [Text]
crsgSubnetIds = lens _crsgSubnetIds (\ s a -> s{_crsgSubnetIds = a}) . _Coerce

instance AWSRequest CreateReplicationSubnetGroup
         where
        type Rs CreateReplicationSubnetGroup =
             CreateReplicationSubnetGroupResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 CreateReplicationSubnetGroupResponse' <$>
                   (x .?> "ReplicationSubnetGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateReplicationSubnetGroup where

instance NFData CreateReplicationSubnetGroup where

instance ToHeaders CreateReplicationSubnetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.CreateReplicationSubnetGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateReplicationSubnetGroup where
        toJSON CreateReplicationSubnetGroup'{..}
          = object
              (catMaybes
                 [("Tags" .=) <$> _crsgTags,
                  Just
                    ("ReplicationSubnetGroupIdentifier" .=
                       _crsgReplicationSubnetGroupIdentifier),
                  Just
                    ("ReplicationSubnetGroupDescription" .=
                       _crsgReplicationSubnetGroupDescription),
                  Just ("SubnetIds" .= _crsgSubnetIds)])

instance ToPath CreateReplicationSubnetGroup where
        toPath = const "/"

instance ToQuery CreateReplicationSubnetGroup where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'createReplicationSubnetGroupResponse' smart constructor.
data CreateReplicationSubnetGroupResponse = CreateReplicationSubnetGroupResponse'
  { _crsgrsReplicationSubnetGroup :: !(Maybe ReplicationSubnetGroup)
  , _crsgrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsgrsReplicationSubnetGroup' - The replication subnet group that was created.
--
-- * 'crsgrsResponseStatus' - -- | The response status code.
createReplicationSubnetGroupResponse
    :: Int -- ^ 'crsgrsResponseStatus'
    -> CreateReplicationSubnetGroupResponse
createReplicationSubnetGroupResponse pResponseStatus_ =
  CreateReplicationSubnetGroupResponse'
    { _crsgrsReplicationSubnetGroup = Nothing
    , _crsgrsResponseStatus = pResponseStatus_
    }


-- | The replication subnet group that was created.
crsgrsReplicationSubnetGroup :: Lens' CreateReplicationSubnetGroupResponse (Maybe ReplicationSubnetGroup)
crsgrsReplicationSubnetGroup = lens _crsgrsReplicationSubnetGroup (\ s a -> s{_crsgrsReplicationSubnetGroup = a})

-- | -- | The response status code.
crsgrsResponseStatus :: Lens' CreateReplicationSubnetGroupResponse Int
crsgrsResponseStatus = lens _crsgrsResponseStatus (\ s a -> s{_crsgrsResponseStatus = a})

instance NFData CreateReplicationSubnetGroupResponse
         where
