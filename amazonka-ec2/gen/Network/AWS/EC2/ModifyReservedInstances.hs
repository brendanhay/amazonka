{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyReservedInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Availability Zone, instance count, instance type, or
-- network platform (EC2-Classic or EC2-VPC) of your Reserved Instances.
-- The Reserved Instances to be modified must be identical, except for
-- Availability Zone, network platform, and instance type.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances>
-- in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyReservedInstances.html>
module Network.AWS.EC2.ModifyReservedInstances
    (
    -- * Request
      ModifyReservedInstances
    -- ** Request constructor
    , modifyReservedInstances
    -- ** Request lenses
    , mrirqClientToken
    , mrirqReservedInstancesIds
    , mrirqTargetConfigurations

    -- * Response
    , ModifyReservedInstancesResponse
    -- ** Response constructor
    , modifyReservedInstancesResponse
    -- ** Response lenses
    , mrirsReservedInstancesModificationId
    , mrirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyReservedInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrirqClientToken'
--
-- * 'mrirqReservedInstancesIds'
--
-- * 'mrirqTargetConfigurations'
data ModifyReservedInstances = ModifyReservedInstances'
    { _mrirqClientToken          :: !(Maybe Text)
    , _mrirqReservedInstancesIds :: ![Text]
    , _mrirqTargetConfigurations :: ![ReservedInstancesConfiguration]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyReservedInstances' smart constructor.
modifyReservedInstances :: ModifyReservedInstances
modifyReservedInstances =
    ModifyReservedInstances'
    { _mrirqClientToken = Nothing
    , _mrirqReservedInstancesIds = mempty
    , _mrirqTargetConfigurations = mempty
    }

-- | A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
mrirqClientToken :: Lens' ModifyReservedInstances (Maybe Text)
mrirqClientToken = lens _mrirqClientToken (\ s a -> s{_mrirqClientToken = a});

-- | The IDs of the Reserved Instances to modify.
mrirqReservedInstancesIds :: Lens' ModifyReservedInstances [Text]
mrirqReservedInstancesIds = lens _mrirqReservedInstancesIds (\ s a -> s{_mrirqReservedInstancesIds = a});

-- | The configuration settings for the Reserved Instances to modify.
mrirqTargetConfigurations :: Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
mrirqTargetConfigurations = lens _mrirqTargetConfigurations (\ s a -> s{_mrirqTargetConfigurations = a});

instance AWSRequest ModifyReservedInstances where
        type Sv ModifyReservedInstances = EC2
        type Rs ModifyReservedInstances =
             ModifyReservedInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ModifyReservedInstancesResponse' <$>
                   (x .@? "reservedInstancesModificationId") <*>
                     (pure (fromEnum s)))

instance ToHeaders ModifyReservedInstances where
        toHeaders = const mempty

instance ToPath ModifyReservedInstances where
        toPath = const "/"

instance ToQuery ModifyReservedInstances where
        toQuery ModifyReservedInstances'{..}
          = mconcat
              ["Action" =:
                 ("ModifyReservedInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ClientToken" =: _mrirqClientToken,
               toQueryList "ReservedInstancesId"
                 _mrirqReservedInstancesIds,
               toQueryList "item" _mrirqTargetConfigurations]

-- | /See:/ 'modifyReservedInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrirsReservedInstancesModificationId'
--
-- * 'mrirsStatus'
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
    { _mrirsReservedInstancesModificationId :: !(Maybe Text)
    , _mrirsStatus                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyReservedInstancesResponse' smart constructor.
modifyReservedInstancesResponse :: Int -> ModifyReservedInstancesResponse
modifyReservedInstancesResponse pStatus_ =
    ModifyReservedInstancesResponse'
    { _mrirsReservedInstancesModificationId = Nothing
    , _mrirsStatus = pStatus_
    }

-- | The ID for the modification.
mrirsReservedInstancesModificationId :: Lens' ModifyReservedInstancesResponse (Maybe Text)
mrirsReservedInstancesModificationId = lens _mrirsReservedInstancesModificationId (\ s a -> s{_mrirsReservedInstancesModificationId = a});

-- | FIXME: Undocumented member.
mrirsStatus :: Lens' ModifyReservedInstancesResponse Int
mrirsStatus = lens _mrirsStatus (\ s a -> s{_mrirsStatus = a});
