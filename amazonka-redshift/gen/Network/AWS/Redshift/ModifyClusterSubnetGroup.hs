{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies a cluster subnet group to include the specified list of VPC
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
    , mcsgrqDescription
    , mcsgrqClusterSubnetGroupName
    , mcsgrqSubnetIds

    -- * Response
    , ModifyClusterSubnetGroupResponse
    -- ** Response constructor
    , modifyClusterSubnetGroupResponse
    -- ** Response lenses
    , mcsgrsClusterSubnetGroup
    , mcsgrsStatus
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
-- * 'mcsgrqDescription'
--
-- * 'mcsgrqClusterSubnetGroupName'
--
-- * 'mcsgrqSubnetIds'
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup'
    { _mcsgrqDescription            :: !(Maybe Text)
    , _mcsgrqClusterSubnetGroupName :: !Text
    , _mcsgrqSubnetIds              :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyClusterSubnetGroup' smart constructor.
modifyClusterSubnetGroup :: Text -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup pClusterSubnetGroupName_ =
    ModifyClusterSubnetGroup'
    { _mcsgrqDescription = Nothing
    , _mcsgrqClusterSubnetGroupName = pClusterSubnetGroupName_
    , _mcsgrqSubnetIds = mempty
    }

-- | A text description of the subnet group to be modified.
mcsgrqDescription :: Lens' ModifyClusterSubnetGroup (Maybe Text)
mcsgrqDescription = lens _mcsgrqDescription (\ s a -> s{_mcsgrqDescription = a});

-- | The name of the subnet group to be modified.
mcsgrqClusterSubnetGroupName :: Lens' ModifyClusterSubnetGroup Text
mcsgrqClusterSubnetGroupName = lens _mcsgrqClusterSubnetGroupName (\ s a -> s{_mcsgrqClusterSubnetGroupName = a});

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgrqSubnetIds :: Lens' ModifyClusterSubnetGroup [Text]
mcsgrqSubnetIds = lens _mcsgrqSubnetIds (\ s a -> s{_mcsgrqSubnetIds = a});

instance AWSRequest ModifyClusterSubnetGroup where
        type Sv ModifyClusterSubnetGroup = Redshift
        type Rs ModifyClusterSubnetGroup =
             ModifyClusterSubnetGroupResponse
        request = post
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
               "Description" =: _mcsgrqDescription,
               "ClusterSubnetGroupName" =:
                 _mcsgrqClusterSubnetGroupName,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _mcsgrqSubnetIds]

-- | /See:/ 'modifyClusterSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrsClusterSubnetGroup'
--
-- * 'mcsgrsStatus'
data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse'
    { _mcsgrsClusterSubnetGroup :: !(Maybe ClusterSubnetGroup)
    , _mcsgrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyClusterSubnetGroupResponse' smart constructor.
modifyClusterSubnetGroupResponse :: Int -> ModifyClusterSubnetGroupResponse
modifyClusterSubnetGroupResponse pStatus_ =
    ModifyClusterSubnetGroupResponse'
    { _mcsgrsClusterSubnetGroup = Nothing
    , _mcsgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
mcsgrsClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
mcsgrsClusterSubnetGroup = lens _mcsgrsClusterSubnetGroup (\ s a -> s{_mcsgrsClusterSubnetGroup = a});

-- | FIXME: Undocumented member.
mcsgrsStatus :: Lens' ModifyClusterSubnetGroupResponse Int
mcsgrsStatus = lens _mcsgrsStatus (\ s a -> s{_mcsgrsStatus = a});
