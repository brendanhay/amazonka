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
-- Module      : Network.AWS.ApplicationAutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a policy for an existing Application Auto Scaling scalable target. Each scalable target is identified by service namespace, a resource ID, and a scalable dimension, and a scaling policy applies to a scalable target that is identified by those three attributes. You cannot create a scaling policy without first registering a scalable target with < RegisterScalableTarget>.
--
-- To update an existing policy, use the existing policy name and set the parameters you want to change. Any existing parameter not changed in an update to an existing policy is not changed in this update request.
--
-- You can view the existing scaling policies for a service namespace with < DescribeScalingPolicies>. If you are no longer using a scaling policy, you can delete it with < DeleteScalingPolicy>.
module Network.AWS.ApplicationAutoScaling.PutScalingPolicy
    (
    -- * Creating a Request
      putScalingPolicy
    , PutScalingPolicy
    -- * Request Lenses
    , pspPolicyType
    , pspStepScalingPolicyConfiguration
    , pspPolicyName
    , pspServiceNamespace
    , pspResourceId
    , pspScalableDimension

    -- * Destructuring the Response
    , putScalingPolicyResponse
    , PutScalingPolicyResponse
    -- * Response Lenses
    , psprsResponseStatus
    , psprsPolicyARN
    ) where

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
    { _pspPolicyType                     :: !(Maybe PolicyType)
    , _pspStepScalingPolicyConfiguration :: !(Maybe StepScalingPolicyConfiguration)
    , _pspPolicyName                     :: !Text
    , _pspServiceNamespace               :: !ServiceNamespace
    , _pspResourceId                     :: !Text
    , _pspScalableDimension              :: !ScalableDimension
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspPolicyType'
--
-- * 'pspStepScalingPolicyConfiguration'
--
-- * 'pspPolicyName'
--
-- * 'pspServiceNamespace'
--
-- * 'pspResourceId'
--
-- * 'pspScalableDimension'
putScalingPolicy
    :: Text -- ^ 'pspPolicyName'
    -> ServiceNamespace -- ^ 'pspServiceNamespace'
    -> Text -- ^ 'pspResourceId'
    -> ScalableDimension -- ^ 'pspScalableDimension'
    -> PutScalingPolicy
putScalingPolicy pPolicyName_ pServiceNamespace_ pResourceId_ pScalableDimension_ =
    PutScalingPolicy'
    { _pspPolicyType = Nothing
    , _pspStepScalingPolicyConfiguration = Nothing
    , _pspPolicyName = pPolicyName_
    , _pspServiceNamespace = pServiceNamespace_
    , _pspResourceId = pResourceId_
    , _pspScalableDimension = pScalableDimension_
    }

-- | The policy type. This parameter is required if you are creating a new policy.
pspPolicyType :: Lens' PutScalingPolicy (Maybe PolicyType)
pspPolicyType = lens _pspPolicyType (\ s a -> s{_pspPolicyType = a});

-- | The configuration for the step scaling policy. This parameter is required if you are creating a new policy. For more information, see <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/ApplicationAutoScaling/latest/APIReference/API_StepScalingPolicyConfiguration.html StepScalingPolicyConfiguration> and <http://docs.aws.amazon.com/http:/docs.aws.amazon.com/ApplicationAutoScaling/latest/APIReference/API_StepAdjustment.html StepAdjustment> in the /Application Auto Scaling API Reference/.
pspStepScalingPolicyConfiguration :: Lens' PutScalingPolicy (Maybe StepScalingPolicyConfiguration)
pspStepScalingPolicyConfiguration = lens _pspStepScalingPolicyConfiguration (\ s a -> s{_pspStepScalingPolicyConfiguration = a});

-- | The name of the scaling policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\ s a -> s{_pspPolicyName = a});

-- | The AWS service namespace of the scalable target that this scaling policy applies to. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the Amazon Web Services General Reference.
pspServiceNamespace :: Lens' PutScalingPolicy ServiceNamespace
pspServiceNamespace = lens _pspServiceNamespace (\ s a -> s{_pspServiceNamespace = a});

-- | The unique resource identifier string for the scalable target that this scaling policy applies to. For Amazon ECS services, this value is the resource type, followed by the cluster name and service name, such as 'service\/default\/sample-webapp'.
pspResourceId :: Lens' PutScalingPolicy Text
pspResourceId = lens _pspResourceId (\ s a -> s{_pspResourceId = a});

-- | The scalable dimension of the scalable target that this scaling policy applies to. The scalable dimension contains the service namespace, resource type, and scaling property, such as 'ecs:service:DesiredCount' for the desired task count of an Amazon ECS service.
pspScalableDimension :: Lens' PutScalingPolicy ScalableDimension
pspScalableDimension = lens _pspScalableDimension (\ s a -> s{_pspScalableDimension = a});

instance AWSRequest PutScalingPolicy where
        type Rs PutScalingPolicy = PutScalingPolicyResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 PutScalingPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "PolicyARN"))

instance Hashable PutScalingPolicy

instance NFData PutScalingPolicy

instance ToHeaders PutScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.PutScalingPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutScalingPolicy where
        toJSON PutScalingPolicy'{..}
          = object
              (catMaybes
                 [("PolicyType" .=) <$> _pspPolicyType,
                  ("StepScalingPolicyConfiguration" .=) <$>
                    _pspStepScalingPolicyConfiguration,
                  Just ("PolicyName" .= _pspPolicyName),
                  Just ("ServiceNamespace" .= _pspServiceNamespace),
                  Just ("ResourceId" .= _pspResourceId),
                  Just ("ScalableDimension" .= _pspScalableDimension)])

instance ToPath PutScalingPolicy where
        toPath = const "/"

instance ToQuery PutScalingPolicy where
        toQuery = const mempty

-- | /See:/ 'putScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
    { _psprsResponseStatus :: !Int
    , _psprsPolicyARN      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsResponseStatus'
--
-- * 'psprsPolicyARN'
putScalingPolicyResponse
    :: Int -- ^ 'psprsResponseStatus'
    -> Text -- ^ 'psprsPolicyARN'
    -> PutScalingPolicyResponse
putScalingPolicyResponse pResponseStatus_ pPolicyARN_ =
    PutScalingPolicyResponse'
    { _psprsResponseStatus = pResponseStatus_
    , _psprsPolicyARN = pPolicyARN_
    }

-- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\ s a -> s{_psprsResponseStatus = a});

-- | The Amazon Resource Name (ARN) of the resulting scaling policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse Text
psprsPolicyARN = lens _psprsPolicyARN (\ s a -> s{_psprsPolicyARN = a});

instance NFData PutScalingPolicyResponse
