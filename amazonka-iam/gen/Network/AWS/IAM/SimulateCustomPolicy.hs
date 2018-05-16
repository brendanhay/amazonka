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
-- Module      : Network.AWS.IAM.SimulateCustomPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate how a set of IAM policies and optionally a resource-based policy works with a list of API operations and AWS resources to determine the policies' effective permissions. The policies are provided as strings.
--
--
-- The simulation does not perform the API operations; it only checks the authorization to determine if the simulated policies allow or deny the operations.
--
-- If you want to simulate existing policies attached to an IAM user, group, or role, use 'SimulatePrincipalPolicy' instead.
--
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the @Condition@ element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use 'GetContextKeysForCustomPolicy' .
--
-- If the output is long, you can use @MaxItems@ and @Marker@ parameters to paginate the results.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.SimulateCustomPolicy
    (
    -- * Creating a Request
      simulateCustomPolicy
    , SimulateCustomPolicy
    -- * Request Lenses
    , scpResourcePolicy
    , scpCallerARN
    , scpResourceHandlingOption
    , scpResourceARNs
    , scpMarker
    , scpMaxItems
    , scpContextEntries
    , scpResourceOwner
    , scpPolicyInputList
    , scpActionNames

    -- * Destructuring the Response
    , simulatePolicyResponse
    , SimulatePolicyResponse
    -- * Response Lenses
    , spEvaluationResults
    , spMarker
    , spIsTruncated
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'simulateCustomPolicy' smart constructor.
data SimulateCustomPolicy = SimulateCustomPolicy'
  { _scpResourcePolicy         :: !(Maybe Text)
  , _scpCallerARN              :: !(Maybe Text)
  , _scpResourceHandlingOption :: !(Maybe Text)
  , _scpResourceARNs           :: !(Maybe [Text])
  , _scpMarker                 :: !(Maybe Text)
  , _scpMaxItems               :: !(Maybe Nat)
  , _scpContextEntries         :: !(Maybe [ContextEntry])
  , _scpResourceOwner          :: !(Maybe Text)
  , _scpPolicyInputList        :: ![Text]
  , _scpActionNames            :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SimulateCustomPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scpResourcePolicy' - A resource-based policy to include in the simulation provided as a string. Each resource in the simulation is treated as if it had this policy attached. You can include only one resource-based policy in a simulation. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
--
-- * 'scpCallerARN' - The ARN of the IAM user that you want to use as the simulated caller of the API operations. @CallerArn@ is required if you include a @ResourcePolicy@ so that the policy's @Principal@ element has a value to use in evaluating the policy. You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
--
-- * 'scpResourceHandlingOption' - Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation. Each of the EC2 scenarios requires that you specify instance, image, and security-group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network-interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .     * __EC2-Classic-InstanceStore__  instance, image, security-group     * __EC2-Classic-EBS__  instance, image, security-group, volume     * __EC2-VPC-InstanceStore__  instance, image, security-group, network-interface     * __EC2-VPC-InstanceStore-Subnet__  instance, image, security-group, network-interface, subnet     * __EC2-VPC-EBS__  instance, image, security-group, network-interface, volume     * __EC2-VPC-EBS-Subnet__  instance, image, security-group, network-interface, subnet, volume
--
-- * 'scpResourceARNs' - A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response. The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter. If you include a @ResourcePolicy@ , then it must be applicable to all of the resources included in the simulation or you receive an invalid input error. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'scpMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'scpMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'scpContextEntries' - A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permission policies, the corresponding value is supplied.
--
-- * 'scpResourceOwner' - An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN, such as an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- * 'scpPolicyInputList' - A list of policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. Do not include any resource-based policies in this parameter. Any resource-based policy must be submitted with the @ResourcePolicy@ parameter. The policies cannot be "scope-down" policies, such as you could include in a call to <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken> or one of the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole> API operations. In other words, do not use policies designed to restrict what a user can do while using the temporary credentials. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
--
-- * 'scpActionNames' - A list of names of API operations to evaluate in the simulation. Each operation is evaluated against each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
simulateCustomPolicy
    :: SimulateCustomPolicy
simulateCustomPolicy =
  SimulateCustomPolicy'
    { _scpResourcePolicy = Nothing
    , _scpCallerARN = Nothing
    , _scpResourceHandlingOption = Nothing
    , _scpResourceARNs = Nothing
    , _scpMarker = Nothing
    , _scpMaxItems = Nothing
    , _scpContextEntries = Nothing
    , _scpResourceOwner = Nothing
    , _scpPolicyInputList = mempty
    , _scpActionNames = mempty
    }


-- | A resource-based policy to include in the simulation provided as a string. Each resource in the simulation is treated as if it had this policy attached. You can include only one resource-based policy in a simulation. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
scpResourcePolicy :: Lens' SimulateCustomPolicy (Maybe Text)
scpResourcePolicy = lens _scpResourcePolicy (\ s a -> s{_scpResourcePolicy = a})

-- | The ARN of the IAM user that you want to use as the simulated caller of the API operations. @CallerArn@ is required if you include a @ResourcePolicy@ so that the policy's @Principal@ element has a value to use in evaluating the policy. You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
scpCallerARN :: Lens' SimulateCustomPolicy (Maybe Text)
scpCallerARN = lens _scpCallerARN (\ s a -> s{_scpCallerARN = a})

-- | Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation. Each of the EC2 scenarios requires that you specify instance, image, and security-group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network-interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .     * __EC2-Classic-InstanceStore__  instance, image, security-group     * __EC2-Classic-EBS__  instance, image, security-group, volume     * __EC2-VPC-InstanceStore__  instance, image, security-group, network-interface     * __EC2-VPC-InstanceStore-Subnet__  instance, image, security-group, network-interface, subnet     * __EC2-VPC-EBS__  instance, image, security-group, network-interface, volume     * __EC2-VPC-EBS-Subnet__  instance, image, security-group, network-interface, subnet, volume
scpResourceHandlingOption :: Lens' SimulateCustomPolicy (Maybe Text)
scpResourceHandlingOption = lens _scpResourceHandlingOption (\ s a -> s{_scpResourceHandlingOption = a})

-- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response. The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter. If you include a @ResourcePolicy@ , then it must be applicable to all of the resources included in the simulation or you receive an invalid input error. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
scpResourceARNs :: Lens' SimulateCustomPolicy [Text]
scpResourceARNs = lens _scpResourceARNs (\ s a -> s{_scpResourceARNs = a}) . _Default . _Coerce

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
scpMarker :: Lens' SimulateCustomPolicy (Maybe Text)
scpMarker = lens _scpMarker (\ s a -> s{_scpMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
scpMaxItems :: Lens' SimulateCustomPolicy (Maybe Natural)
scpMaxItems = lens _scpMaxItems (\ s a -> s{_scpMaxItems = a}) . mapping _Nat

-- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permission policies, the corresponding value is supplied.
scpContextEntries :: Lens' SimulateCustomPolicy [ContextEntry]
scpContextEntries = lens _scpContextEntries (\ s a -> s{_scpContextEntries = a}) . _Default . _Coerce

-- | An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN, such as an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
scpResourceOwner :: Lens' SimulateCustomPolicy (Maybe Text)
scpResourceOwner = lens _scpResourceOwner (\ s a -> s{_scpResourceOwner = a})

-- | A list of policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. Do not include any resource-based policies in this parameter. Any resource-based policy must be submitted with the @ResourcePolicy@ parameter. The policies cannot be "scope-down" policies, such as you could include in a call to <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken> or one of the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole> API operations. In other words, do not use policies designed to restrict what a user can do while using the temporary credentials. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:     * Any printable ASCII character ranging from the space character (\u0020) through the end of the ASCII character range     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF)     * The special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D)
scpPolicyInputList :: Lens' SimulateCustomPolicy [Text]
scpPolicyInputList = lens _scpPolicyInputList (\ s a -> s{_scpPolicyInputList = a}) . _Coerce

-- | A list of names of API operations to evaluate in the simulation. Each operation is evaluated against each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
scpActionNames :: Lens' SimulateCustomPolicy [Text]
scpActionNames = lens _scpActionNames (\ s a -> s{_scpActionNames = a}) . _Coerce

instance AWSPager SimulateCustomPolicy where
        page rq rs
          | stop (rs ^. spIsTruncated) = Nothing
          | isNothing (rs ^. spMarker) = Nothing
          | otherwise = Just $ rq & scpMarker .~ rs ^. spMarker

instance AWSRequest SimulateCustomPolicy where
        type Rs SimulateCustomPolicy = SimulatePolicyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "SimulateCustomPolicyResult"
              (\ s h x -> parseXML x)

instance Hashable SimulateCustomPolicy where

instance NFData SimulateCustomPolicy where

instance ToHeaders SimulateCustomPolicy where
        toHeaders = const mempty

instance ToPath SimulateCustomPolicy where
        toPath = const "/"

instance ToQuery SimulateCustomPolicy where
        toQuery SimulateCustomPolicy'{..}
          = mconcat
              ["Action" =: ("SimulateCustomPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ResourcePolicy" =: _scpResourcePolicy,
               "CallerArn" =: _scpCallerARN,
               "ResourceHandlingOption" =:
                 _scpResourceHandlingOption,
               "ResourceArns" =:
                 toQuery (toQueryList "member" <$> _scpResourceARNs),
               "Marker" =: _scpMarker, "MaxItems" =: _scpMaxItems,
               "ContextEntries" =:
                 toQuery
                   (toQueryList "member" <$> _scpContextEntries),
               "ResourceOwner" =: _scpResourceOwner,
               "PolicyInputList" =:
                 toQueryList "member" _scpPolicyInputList,
               "ActionNames" =:
                 toQueryList "member" _scpActionNames]
