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
-- Module      : Network.AWS.IAM.SimulatePrincipalPolicy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate how a set of IAM policies attached to an IAM entity works with a list of API actions and AWS resources to determine the policies\' effective permissions. The entity can be an IAM user, group, or role. If you specify a user, then the simulation also includes all of the policies that are attached to groups that the user belongs to .
--
-- You can optionally include a list of one or more additional policies specified as strings to include in the simulation. If you want to simulate only policies specified as strings, use < SimulateCustomPolicy> instead.
--
-- You can also optionally include one resource-based policy to be evaluated with each of the resources included in the simulation.
--
-- The simulation does not perform the API actions, it only checks the authorization to determine if the simulated policies allow or deny the actions.
--
-- __Note:__ This API discloses information about the permissions granted to other users. If you do not want users to see other user\'s permissions, then consider allowing them to use < SimulateCustomPolicy> instead.
--
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the 'Condition' element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use < GetContextKeysForPrincipalPolicy>.
--
-- If the output is long, you can use the 'MaxItems' and 'Marker' parameters to paginate the results.
module Network.AWS.IAM.SimulatePrincipalPolicy
    (
    -- * Creating a Request
      simulatePrincipalPolicy
    , SimulatePrincipalPolicy
    -- * Request Lenses
    , sppPolicyInputList
    , sppResourcePolicy
    , sppCallerARN
    , sppResourceHandlingOption
    , sppResourceARNs
    , sppMarker
    , sppMaxItems
    , sppContextEntries
    , sppResourceOwner
    , sppPolicySourceARN
    , sppActionNames

    -- * Destructuring the Response
    , simulatePolicyResponse
    , SimulatePolicyResponse
    -- * Response Lenses
    , spEvaluationResults
    , spMarker
    , spIsTruncated
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'simulatePrincipalPolicy' smart constructor.
data SimulatePrincipalPolicy = SimulatePrincipalPolicy'
    { _sppPolicyInputList        :: !(Maybe [Text])
    , _sppResourcePolicy         :: !(Maybe Text)
    , _sppCallerARN              :: !(Maybe Text)
    , _sppResourceHandlingOption :: !(Maybe Text)
    , _sppResourceARNs           :: !(Maybe [Text])
    , _sppMarker                 :: !(Maybe Text)
    , _sppMaxItems               :: !(Maybe Nat)
    , _sppContextEntries         :: !(Maybe [ContextEntry])
    , _sppResourceOwner          :: !(Maybe Text)
    , _sppPolicySourceARN        :: !Text
    , _sppActionNames            :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SimulatePrincipalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sppPolicyInputList'
--
-- * 'sppResourcePolicy'
--
-- * 'sppCallerARN'
--
-- * 'sppResourceHandlingOption'
--
-- * 'sppResourceARNs'
--
-- * 'sppMarker'
--
-- * 'sppMaxItems'
--
-- * 'sppContextEntries'
--
-- * 'sppResourceOwner'
--
-- * 'sppPolicySourceARN'
--
-- * 'sppActionNames'
simulatePrincipalPolicy
    :: Text -- ^ 'sppPolicySourceARN'
    -> SimulatePrincipalPolicy
simulatePrincipalPolicy pPolicySourceARN_ =
    SimulatePrincipalPolicy'
    { _sppPolicyInputList = Nothing
    , _sppResourcePolicy = Nothing
    , _sppCallerARN = Nothing
    , _sppResourceHandlingOption = Nothing
    , _sppResourceARNs = Nothing
    , _sppMarker = Nothing
    , _sppMaxItems = Nothing
    , _sppContextEntries = Nothing
    , _sppResourceOwner = Nothing
    , _sppPolicySourceARN = pPolicySourceARN_
    , _sppActionNames = mempty
    }

-- | An optional list of additional policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
sppPolicyInputList :: Lens' SimulatePrincipalPolicy [Text]
sppPolicyInputList = lens _sppPolicyInputList (\ s a -> s{_sppPolicyInputList = a}) . _Default . _Coerce;

-- | A resource-based policy to include in the simulation provided as a string. Each resource in the simulation is treated as if it had this policy attached. You can include only one resource-based policy in a simulation.
sppResourcePolicy :: Lens' SimulatePrincipalPolicy (Maybe Text)
sppResourcePolicy = lens _sppResourcePolicy (\ s a -> s{_sppResourcePolicy = a});

-- | The ARN of the user that you want to specify as the simulated caller of the APIs. If you do not specify a 'CallerArn', it defaults to the ARN of the user that you specify in 'PolicySourceArn', if you specified a user. If you include both a 'PolicySourceArn' (for example, 'arn:aws:iam::123456789012:user\/David') and a 'CallerArn' (for example, 'arn:aws:iam::123456789012:user\/Bob'), the result is that you simulate calling the APIs as Bob, as if Bob had David\'s policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
--
-- 'CallerArn' is required if you include a 'ResourcePolicy' and the 'PolicySourceArn' is not the ARN for an IAM user. This is required so that the resource-based policy\'s 'Principal' element has a value to use in evaluating the policy.
sppCallerARN :: Lens' SimulatePrincipalPolicy (Maybe Text)
sppCallerARN = lens _sppCallerARN (\ s a -> s{_sppCallerARN = a});

-- | Specifies the type of simulation to run. Different APIs that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and security-group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network-interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /AWS EC2 User Guide/.
--
-- -   __EC2-Classic-InstanceStore__
--
--     instance, image, security-group
--
-- -   __EC2-Classic-EBS__
--
--     instance, image, security-group, volume
--
-- -   __EC2-VPC-InstanceStore__
--
--     instance, image, security-group, network-interface
--
-- -   __EC2-VPC-InstanceStore-Subnet__
--
--     instance, image, security-group, network-interface, subnet
--
-- -   __EC2-VPC-EBS__
--
--     instance, image, security-group, network-interface, volume
--
-- -   __EC2-VPC-EBS-Subnet__
--
--     instance, image, security-group, network-interface, subnet, volume
--
sppResourceHandlingOption :: Lens' SimulatePrincipalPolicy (Maybe Text)
sppResourceHandlingOption = lens _sppResourceHandlingOption (\ s a -> s{_sppResourceHandlingOption = a});

-- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided then the value defaults to '*' (all resources). Each API in the 'ActionNames' parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the 'ResourcePolicy' parameter.
sppResourceARNs :: Lens' SimulatePrincipalPolicy [Text]
sppResourceARNs = lens _sppResourceARNs (\ s a -> s{_sppResourceARNs = a}) . _Default . _Coerce;

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the 'Marker' element in the response that you received to indicate where the next call should start.
sppMarker :: Lens' SimulatePrincipalPolicy (Maybe Text)
sppMarker = lens _sppMarker (\ s a -> s{_sppMarker = a});

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the 'IsTruncated' response element returns 'true' and 'Marker' contains a value to include in the subsequent call that tells the service where to continue from.
sppMaxItems :: Lens' SimulatePrincipalPolicy (Maybe Natural)
sppMaxItems = lens _sppMaxItems (\ s a -> s{_sppMaxItems = a}) . mapping _Nat;

-- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated by a 'Condition' element in one of the simulated policies, the corresponding value is supplied.
sppContextEntries :: Lens' SimulatePrincipalPolicy [ContextEntry]
sppContextEntries = lens _sppContextEntries (\ s a -> s{_sppContextEntries = a}) . _Default . _Coerce;

-- | An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN, such as an S3 bucket or object. If 'ResourceOwner' is specified, it is also used as the account owner of any 'ResourcePolicy' included in the simulation. If the 'ResourceOwner' parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in 'CallerArn'. This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user 'CallerArn'.
sppResourceOwner :: Lens' SimulatePrincipalPolicy (Maybe Text)
sppResourceOwner = lens _sppResourceOwner (\ s a -> s{_sppResourceOwner = a});

-- | The Amazon Resource Name (ARN) of a user, group, or role whose policies you want to include in the simulation. If you specify a user, group, or role, the simulation includes all policies that are associated with that entity. If you specify a user, the simulation also includes all policies that are attached to any groups the user belongs to.
sppPolicySourceARN :: Lens' SimulatePrincipalPolicy Text
sppPolicySourceARN = lens _sppPolicySourceARN (\ s a -> s{_sppPolicySourceARN = a});

-- | A list of names of API actions to evaluate in the simulation. Each action is evaluated for each resource. Each action must include the service identifier, such as 'iam:CreateUser'.
sppActionNames :: Lens' SimulatePrincipalPolicy [Text]
sppActionNames = lens _sppActionNames (\ s a -> s{_sppActionNames = a}) . _Coerce;

instance AWSRequest SimulatePrincipalPolicy where
        type Rs SimulatePrincipalPolicy =
             SimulatePolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "SimulatePrincipalPolicyResult"
              (\ s h x -> parseXML x)

instance Hashable SimulatePrincipalPolicy

instance NFData SimulatePrincipalPolicy

instance ToHeaders SimulatePrincipalPolicy where
        toHeaders = const mempty

instance ToPath SimulatePrincipalPolicy where
        toPath = const "/"

instance ToQuery SimulatePrincipalPolicy where
        toQuery SimulatePrincipalPolicy'{..}
          = mconcat
              ["Action" =:
                 ("SimulatePrincipalPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyInputList" =:
                 toQuery
                   (toQueryList "member" <$> _sppPolicyInputList),
               "ResourcePolicy" =: _sppResourcePolicy,
               "CallerArn" =: _sppCallerARN,
               "ResourceHandlingOption" =:
                 _sppResourceHandlingOption,
               "ResourceArns" =:
                 toQuery (toQueryList "member" <$> _sppResourceARNs),
               "Marker" =: _sppMarker, "MaxItems" =: _sppMaxItems,
               "ContextEntries" =:
                 toQuery
                   (toQueryList "member" <$> _sppContextEntries),
               "ResourceOwner" =: _sppResourceOwner,
               "PolicySourceArn" =: _sppPolicySourceARN,
               "ActionNames" =:
                 toQueryList "member" _sppActionNames]
