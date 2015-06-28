{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the
-- new list of subnets.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifyClusterSubnetGroup.html>
module Network.AWS.Redshift.ModifyClusterSubnetGroup
    (
    -- * Request
      ModifyClusterSubnetGroup
    -- ** Request constructor
    , modifyClusterSubnetGroup
    -- ** Request lenses
    , mcsgDescription
    , mcsgClusterSubnetGroupName
    , mcsgSubnetIds

    -- * Response
    , ModifyClusterSubnetGroupResponse
    -- ** Response constructor
    , modifyClusterSubnetGroupResponse
    -- ** Response lenses
    , mcsgrClusterSubnetGroup
    , mcsgrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyClusterSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgDescription'
--
-- * 'mcsgClusterSubnetGroupName'
--
-- * 'mcsgSubnetIds'
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
    { _mcsgDescription            :: !(Maybe Text)
    , _mcsgClusterSubnetGroupName :: !Text
    , _mcsgSubnetIds              :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'ModifyClusterSubnetGroup' smart constructor.
modifyClusterSubnetGroup :: Text -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup pClusterSubnetGroupName =
    ModifyClusterSubnetGroup'
    { _mcsgDescription = Nothing
    , _mcsgClusterSubnetGroupName = pClusterSubnetGroupName
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
mcsgSubnetIds = lens _mcsgSubnetIds (\ s a -> s{_mcsgSubnetIds = a});

instance AWSRequest ModifyClusterSubnetGroup where
        type Sv ModifyClusterSubnetGroup = Redshift
        type Rs ModifyClusterSubnetGroup =
             ModifyClusterSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "ModifyClusterSubnetGroupResult"
              (\ s h x ->
                 ModifyClusterSubnetGroupResponse' <$>
                   (x .@? "ClusterSubnetGroup") <*> (pure s))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrClusterSubnetGroup'
--
-- * 'mcsgrStatus'
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
    { _mcsgrClusterSubnetGroup :: !(Maybe ClusterSubnetGroup)
    , _mcsgrStatus             :: !Status
    } deriving (Eq,Read,Show)

-- | 'ModifyClusterSubnetGroupResponse' smart constructor.
modifyClusterSubnetGroupResponse :: Status -> ModifyClusterSubnetGroupResponse
modifyClusterSubnetGroupResponse pStatus =
    ModifyClusterSubnetGroupResponse'
    { _mcsgrClusterSubnetGroup = Nothing
    , _mcsgrStatus = pStatus
    }

-- | FIXME: Undocumented member.
mcsgrClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
mcsgrClusterSubnetGroup = lens _mcsgrClusterSubnetGroup (\ s a -> s{_mcsgrClusterSubnetGroup = a});

-- | FIXME: Undocumented member.
mcsgrStatus :: Lens' ModifyClusterSubnetGroupResponse Status
mcsgrStatus = lens _mcsgrStatus (\ s a -> s{_mcsgrStatus = a});
