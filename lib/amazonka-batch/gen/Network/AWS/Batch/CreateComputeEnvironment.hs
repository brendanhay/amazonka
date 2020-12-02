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
-- Module      : Network.AWS.Batch.CreateComputeEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Batch compute environment. You can create @MANAGED@ or @UNMANAGED@ compute environments.
--
--
-- In a managed compute environment, AWS Batch manages the compute resources within the environment, based on the compute resources that you specify. Instances launched into a managed compute environment use a recent, approved version of the Amazon ECS-optimized AMI. You can choose to use Amazon EC2 On-Demand Instances in your managed compute environment, or you can use Amazon EC2 Spot Instances that only launch when the Spot bid price is below a specified percentage of the On-Demand price.
--
-- In an unmanaged compute environment, you can manage your own compute resources. This provides more compute resource configuration options, such as using a custom AMI, but you must ensure that your AMI meets the Amazon ECS container instance AMI specification. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html Container Instance AMIs> in the /Amazon Elastic Container Service Developer Guide/ . After you have created your unmanaged compute environment, you can use the 'DescribeComputeEnvironments' operation to find the Amazon ECS cluster that is associated with it and then manually launch your container instances into that Amazon ECS cluster. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html Launching an Amazon ECS Container Instance> in the /Amazon Elastic Container Service Developer Guide/ .
--
module Network.AWS.Batch.CreateComputeEnvironment
    (
    -- * Creating a Request
      createComputeEnvironment
    , CreateComputeEnvironment
    -- * Request Lenses
    , cceState
    , cceComputeResources
    , cceComputeEnvironmentName
    , cceType
    , cceServiceRole

    -- * Destructuring the Response
    , createComputeEnvironmentResponse
    , CreateComputeEnvironmentResponse
    -- * Response Lenses
    , ccersComputeEnvironmentName
    , ccersComputeEnvironmentARN
    , ccersResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createComputeEnvironment' smart constructor.
data CreateComputeEnvironment = CreateComputeEnvironment'
  { _cceState                  :: !(Maybe CEState)
  , _cceComputeResources       :: !(Maybe ComputeResource)
  , _cceComputeEnvironmentName :: !Text
  , _cceType                   :: !CEType
  , _cceServiceRole            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateComputeEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cceState' - The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
--
-- * 'cceComputeResources' - Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments.
--
-- * 'cceComputeEnvironmentName' - The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- * 'cceType' - The type of the compute environment.
--
-- * 'cceServiceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf. If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
createComputeEnvironment
    :: Text -- ^ 'cceComputeEnvironmentName'
    -> CEType -- ^ 'cceType'
    -> Text -- ^ 'cceServiceRole'
    -> CreateComputeEnvironment
createComputeEnvironment pComputeEnvironmentName_ pType_ pServiceRole_ =
  CreateComputeEnvironment'
    { _cceState = Nothing
    , _cceComputeResources = Nothing
    , _cceComputeEnvironmentName = pComputeEnvironmentName_
    , _cceType = pType_
    , _cceServiceRole = pServiceRole_
    }


-- | The state of the compute environment. If the state is @ENABLED@ , then the compute environment accepts jobs from a queue and can scale out automatically based on queues.
cceState :: Lens' CreateComputeEnvironment (Maybe CEState)
cceState = lens _cceState (\ s a -> s{_cceState = a})

-- | Details of the compute resources managed by the compute environment. This parameter is required for managed compute environments.
cceComputeResources :: Lens' CreateComputeEnvironment (Maybe ComputeResource)
cceComputeResources = lens _cceComputeResources (\ s a -> s{_cceComputeResources = a})

-- | The name for your compute environment. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
cceComputeEnvironmentName :: Lens' CreateComputeEnvironment Text
cceComputeEnvironmentName = lens _cceComputeEnvironmentName (\ s a -> s{_cceComputeEnvironmentName = a})

-- | The type of the compute environment.
cceType :: Lens' CreateComputeEnvironment CEType
cceType = lens _cceType (\ s a -> s{_cceType = a})

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf. If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
cceServiceRole :: Lens' CreateComputeEnvironment Text
cceServiceRole = lens _cceServiceRole (\ s a -> s{_cceServiceRole = a})

instance AWSRequest CreateComputeEnvironment where
        type Rs CreateComputeEnvironment =
             CreateComputeEnvironmentResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 CreateComputeEnvironmentResponse' <$>
                   (x .?> "computeEnvironmentName") <*>
                     (x .?> "computeEnvironmentArn")
                     <*> (pure (fromEnum s)))

instance Hashable CreateComputeEnvironment where

instance NFData CreateComputeEnvironment where

instance ToHeaders CreateComputeEnvironment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateComputeEnvironment where
        toJSON CreateComputeEnvironment'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _cceState,
                  ("computeResources" .=) <$> _cceComputeResources,
                  Just
                    ("computeEnvironmentName" .=
                       _cceComputeEnvironmentName),
                  Just ("type" .= _cceType),
                  Just ("serviceRole" .= _cceServiceRole)])

instance ToPath CreateComputeEnvironment where
        toPath = const "/v1/createcomputeenvironment"

instance ToQuery CreateComputeEnvironment where
        toQuery = const mempty

-- | /See:/ 'createComputeEnvironmentResponse' smart constructor.
data CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse'
  { _ccersComputeEnvironmentName :: !(Maybe Text)
  , _ccersComputeEnvironmentARN  :: !(Maybe Text)
  , _ccersResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateComputeEnvironmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccersComputeEnvironmentName' - The name of the compute environment.
--
-- * 'ccersComputeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
--
-- * 'ccersResponseStatus' - -- | The response status code.
createComputeEnvironmentResponse
    :: Int -- ^ 'ccersResponseStatus'
    -> CreateComputeEnvironmentResponse
createComputeEnvironmentResponse pResponseStatus_ =
  CreateComputeEnvironmentResponse'
    { _ccersComputeEnvironmentName = Nothing
    , _ccersComputeEnvironmentARN = Nothing
    , _ccersResponseStatus = pResponseStatus_
    }


-- | The name of the compute environment.
ccersComputeEnvironmentName :: Lens' CreateComputeEnvironmentResponse (Maybe Text)
ccersComputeEnvironmentName = lens _ccersComputeEnvironmentName (\ s a -> s{_ccersComputeEnvironmentName = a})

-- | The Amazon Resource Name (ARN) of the compute environment.
ccersComputeEnvironmentARN :: Lens' CreateComputeEnvironmentResponse (Maybe Text)
ccersComputeEnvironmentARN = lens _ccersComputeEnvironmentARN (\ s a -> s{_ccersComputeEnvironmentARN = a})

-- | -- | The response status code.
ccersResponseStatus :: Lens' CreateComputeEnvironmentResponse Int
ccersResponseStatus = lens _ccersResponseStatus (\ s a -> s{_ccersResponseStatus = a})

instance NFData CreateComputeEnvironmentResponse
         where
