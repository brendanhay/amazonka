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
-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the
-- new list of subnets.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifyClusterSubnetGroup.html AWS API Reference> for ModifyClusterSubnetGroup.
module Network.AWS.Redshift.ModifyClusterSubnetGroup
    (
    -- * Creating a Request
      modifyClusterSubnetGroup
    , ModifyClusterSubnetGroup
    -- * Request Lenses
    , mcsgDescription
    , mcsgClusterSubnetGroupName
    , mcsgSubnetIds

    -- * Destructuring the Response
    , modifyClusterSubnetGroupResponse
    , ModifyClusterSubnetGroupResponse
    -- * Response Lenses
    , mcsgrsClusterSubnetGroup
    , mcsgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyClusterSubnetGroup' smart constructor.
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
    { _mcsgDescription            :: !(Maybe Text)
    , _mcsgClusterSubnetGroupName :: !Text
    , _mcsgSubnetIds              :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyClusterSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsgDescription'
--
-- * 'mcsgClusterSubnetGroupName'
--
-- * 'mcsgSubnetIds'
modifyClusterSubnetGroup
    :: Text -- ^ 'mcsgClusterSubnetGroupName'
    -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup pClusterSubnetGroupName_ =
    ModifyClusterSubnetGroup'
    { _mcsgDescription = Nothing
    , _mcsgClusterSubnetGroupName = pClusterSubnetGroupName_
    , _mcsgSubnetIds = mempty
    }

-- | A text description of the subnet group to be modified.
mcsgDescription :: Lens' ModifyClusterSubnetGroup (Maybe Text)
mcsgDescription = lens _mcsgDescription (\ s a -> s{_mcsgDescription = a});

-- | The name of the subnet group to be modified.
mcsgClusterSubnetGroupName :: Lens' ModifyClusterSubnetGroup Text
mcsgClusterSubnetGroupName = lens _mcsgClusterSubnetGroupName (\ s a -> s{_mcsgClusterSubnetGroupName = a});

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgSubnetIds :: Lens' ModifyClusterSubnetGroup [Text]
mcsgSubnetIds = lens _mcsgSubnetIds (\ s a -> s{_mcsgSubnetIds = a}) . _Coerce;

instance AWSRequest ModifyClusterSubnetGroup where
        type Rs ModifyClusterSubnetGroup =
             ModifyClusterSubnetGroupResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "ModifyClusterSubnetGroupResult"
              (\ s h x ->
                 ModifyClusterSubnetGroupResponse' <$>
                   (x .@? "ClusterSubnetGroup") <*> (pure (fromEnum s)))

instance ToHeaders ModifyClusterSubnetGroup where
        toHeaders = const mempty

instance ToPath ModifyClusterSubnetGroup where
        toPath = const "/"

instance ToQuery ModifyClusterSubnetGroup where
        toQuery ModifyClusterSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyClusterSubnetGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Description" =: _mcsgDescription,
               "ClusterSubnetGroupName" =:
                 _mcsgClusterSubnetGroupName,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _mcsgSubnetIds]

-- | /See:/ 'modifyClusterSubnetGroupResponse' smart constructor.
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
    { _mcsgrsClusterSubnetGroup :: !(Maybe ClusterSubnetGroup)
    , _mcsgrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyClusterSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsgrsClusterSubnetGroup'
--
-- * 'mcsgrsStatus'
modifyClusterSubnetGroupResponse
    :: Int -- ^ 'mcsgrsStatus'
    -> ModifyClusterSubnetGroupResponse
modifyClusterSubnetGroupResponse pStatus_ =
    ModifyClusterSubnetGroupResponse'
    { _mcsgrsClusterSubnetGroup = Nothing
    , _mcsgrsStatus = pStatus_
    }

-- | Undocumented member.
mcsgrsClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
mcsgrsClusterSubnetGroup = lens _mcsgrsClusterSubnetGroup (\ s a -> s{_mcsgrsClusterSubnetGroup = a});

-- | The response status code.
mcsgrsStatus :: Lens' ModifyClusterSubnetGroupResponse Int
mcsgrsStatus = lens _mcsgrsStatus (\ s a -> s{_mcsgrsStatus = a});
