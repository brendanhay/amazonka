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
-- Module      : Network.AWS.EC2.ModifyReservedInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Availability Zone, instance count, instance type, or network platform (EC2-Classic or EC2-VPC) of your Reserved Instances. The Reserved Instances to be modified must be identical, except for Availability Zone, network platform, and instance type.
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the Amazon Elastic Compute Cloud User Guide.
--
module Network.AWS.EC2.ModifyReservedInstances
    (
    -- * Creating a Request
      modifyReservedInstances
    , ModifyReservedInstances
    -- * Request Lenses
    , mriClientToken
    , mriReservedInstancesIds
    , mriTargetConfigurations

    -- * Destructuring the Response
    , modifyReservedInstancesResponse
    , ModifyReservedInstancesResponse
    -- * Response Lenses
    , mrirsReservedInstancesModificationId
    , mrirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyReservedInstances.
--
--
--
-- /See:/ 'modifyReservedInstances' smart constructor.
data ModifyReservedInstances = ModifyReservedInstances'
  { _mriClientToken          :: !(Maybe Text)
  , _mriReservedInstancesIds :: ![Text]
  , _mriTargetConfigurations :: ![ReservedInstancesConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReservedInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mriClientToken' - A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'mriReservedInstancesIds' - The IDs of the Reserved Instances to modify.
--
-- * 'mriTargetConfigurations' - The configuration settings for the Reserved Instances to modify.
modifyReservedInstances
    :: ModifyReservedInstances
modifyReservedInstances =
  ModifyReservedInstances'
    { _mriClientToken = Nothing
    , _mriReservedInstancesIds = mempty
    , _mriTargetConfigurations = mempty
    }


-- | A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
mriClientToken :: Lens' ModifyReservedInstances (Maybe Text)
mriClientToken = lens _mriClientToken (\ s a -> s{_mriClientToken = a})

-- | The IDs of the Reserved Instances to modify.
mriReservedInstancesIds :: Lens' ModifyReservedInstances [Text]
mriReservedInstancesIds = lens _mriReservedInstancesIds (\ s a -> s{_mriReservedInstancesIds = a}) . _Coerce

-- | The configuration settings for the Reserved Instances to modify.
mriTargetConfigurations :: Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
mriTargetConfigurations = lens _mriTargetConfigurations (\ s a -> s{_mriTargetConfigurations = a}) . _Coerce

instance AWSRequest ModifyReservedInstances where
        type Rs ModifyReservedInstances =
             ModifyReservedInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyReservedInstancesResponse' <$>
                   (x .@? "reservedInstancesModificationId") <*>
                     (pure (fromEnum s)))

instance Hashable ModifyReservedInstances where

instance NFData ModifyReservedInstances where

instance ToHeaders ModifyReservedInstances where
        toHeaders = const mempty

instance ToPath ModifyReservedInstances where
        toPath = const "/"

instance ToQuery ModifyReservedInstances where
        toQuery ModifyReservedInstances'{..}
          = mconcat
              ["Action" =:
                 ("ModifyReservedInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _mriClientToken,
               toQueryList "ReservedInstancesId"
                 _mriReservedInstancesIds,
               toQueryList
                 "ReservedInstancesConfigurationSetItemType"
                 _mriTargetConfigurations]

-- | Contains the output of ModifyReservedInstances.
--
--
--
-- /See:/ 'modifyReservedInstancesResponse' smart constructor.
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
  { _mrirsReservedInstancesModificationId :: !(Maybe Text)
  , _mrirsResponseStatus                  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReservedInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrirsReservedInstancesModificationId' - The ID for the modification.
--
-- * 'mrirsResponseStatus' - -- | The response status code.
modifyReservedInstancesResponse
    :: Int -- ^ 'mrirsResponseStatus'
    -> ModifyReservedInstancesResponse
modifyReservedInstancesResponse pResponseStatus_ =
  ModifyReservedInstancesResponse'
    { _mrirsReservedInstancesModificationId = Nothing
    , _mrirsResponseStatus = pResponseStatus_
    }


-- | The ID for the modification.
mrirsReservedInstancesModificationId :: Lens' ModifyReservedInstancesResponse (Maybe Text)
mrirsReservedInstancesModificationId = lens _mrirsReservedInstancesModificationId (\ s a -> s{_mrirsReservedInstancesModificationId = a})

-- | -- | The response status code.
mrirsResponseStatus :: Lens' ModifyReservedInstancesResponse Int
mrirsResponseStatus = lens _mrirsResponseStatus (\ s a -> s{_mrirsResponseStatus = a})

instance NFData ModifyReservedInstancesResponse where
