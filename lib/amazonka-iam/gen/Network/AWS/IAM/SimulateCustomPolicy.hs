{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SimulateCustomPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate how a set of IAM policies and optionally a resource-based policy works with a list of API operations and AWS resources to determine the policies' effective permissions. The policies are provided as strings.
--
-- The simulation does not perform the API operations; it only checks the authorization to determine if the simulated policies allow or deny the operations.
-- If you want to simulate existing policies that are attached to an IAM user, group, or role, use 'SimulatePrincipalPolicy' instead.
-- Context keys are variables that are maintained by AWS and its services and which provide details about the context of an API query request. You can use the @Condition@ element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use 'GetContextKeysForCustomPolicy' .
-- If the output is long, you can use @MaxItems@ and @Marker@ parameters to paginate the results.
--
-- This operation returns paginated results.
module Network.AWS.IAM.SimulateCustomPolicy
  ( -- * Creating a request
    SimulateCustomPolicy (..),
    mkSimulateCustomPolicy,

    -- ** Request lenses
    scpResourcePolicy,
    scpCallerARN,
    scpResourceHandlingOption,
    scpResourceARNs,
    scpPermissionsBoundaryPolicyInputList,
    scpMarker,
    scpMaxItems,
    scpContextEntries,
    scpResourceOwner,
    scpPolicyInputList,
    scpActionNames,

    -- * Destructuring the response
    SimulatePolicyResponse (..),
    mkSimulatePolicyResponse,

    -- ** Response lenses
    spEvaluationResults,
    spMarker,
    spIsTruncated,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSimulateCustomPolicy' smart constructor.
data SimulateCustomPolicy = SimulateCustomPolicy'
  { resourcePolicy ::
      Lude.Maybe Lude.Text,
    callerARN :: Lude.Maybe Lude.Text,
    resourceHandlingOption :: Lude.Maybe Lude.Text,
    resourceARNs :: Lude.Maybe [Lude.Text],
    permissionsBoundaryPolicyInputList ::
      Lude.Maybe [Lude.Text],
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural,
    contextEntries :: Lude.Maybe [ContextEntry],
    resourceOwner :: Lude.Maybe Lude.Text,
    policyInputList :: [Lude.Text],
    actionNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimulateCustomPolicy' with the minimum fields required to make a request.
--
-- * 'actionNames' - A list of names of API operations to evaluate in the simulation. Each operation is evaluated against each resource. Each operation must include the service identifier, such as @iam:CreateUser@ . This operation does not support using wildcards (*) in an action name.
-- * 'callerARN' - The ARN of the IAM user that you want to use as the simulated caller of the API operations. @CallerArn@ is required if you include a @ResourcePolicy@ so that the policy's @Principal@ element has a value to use in evaluating the policy.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
-- * 'contextEntries' - A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'permissionsBoundaryPolicyInputList' - The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that an IAM entity can have. You can input only one permissions boundary when you pass a policy to this operation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string that contains the complete, valid JSON text of a permissions boundary policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- * 'policyInputList' - A list of policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. Do not include any resource-based policies in this parameter. Any resource-based policy must be submitted with the @ResourcePolicy@ parameter. The policies cannot be "scope-down" policies, such as you could include in a call to <https://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken> or one of the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole> API operations. In other words, do not use policies designed to restrict what a user can do while using the temporary credentials.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- * 'resourceARNs' - A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- If you include a @ResourcePolicy@ , then it must be applicable to all of the resources included in the simulation or you receive an invalid input error.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'resourceHandlingOption' - Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and security-group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network-interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .
--
--     * __EC2-Classic-InstanceStore__
-- instance, image, security-group
--
--
--     * __EC2-Classic-EBS__
-- instance, image, security-group, volume
--
--
--     * __EC2-VPC-InstanceStore__
-- instance, image, security-group, network-interface
--
--
--     * __EC2-VPC-InstanceStore-Subnet__
-- instance, image, security-group, network-interface, subnet
--
--
--     * __EC2-VPC-EBS__
-- instance, image, security-group, network-interface, volume
--
--
--     * __EC2-VPC-EBS-Subnet__
-- instance, image, security-group, network-interface, subnet, volume
--
--
-- * 'resourceOwner' - An ARN representing the AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- The ARN for an account uses the following syntax: @arn:aws:iam::/AWS-account-ID/ :root@ . For example, to represent the account with the 112233445566 ID, use the following ARN: @arn:aws:iam::112233445566-ID:root@ .
-- * 'resourcePolicy' - A resource-based policy to include in the simulation provided as a string. Each resource in the simulation is treated as if it had this policy attached. You can include only one resource-based policy in a simulation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
mkSimulateCustomPolicy ::
  SimulateCustomPolicy
mkSimulateCustomPolicy =
  SimulateCustomPolicy'
    { resourcePolicy = Lude.Nothing,
      callerARN = Lude.Nothing,
      resourceHandlingOption = Lude.Nothing,
      resourceARNs = Lude.Nothing,
      permissionsBoundaryPolicyInputList = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      contextEntries = Lude.Nothing,
      resourceOwner = Lude.Nothing,
      policyInputList = Lude.mempty,
      actionNames = Lude.mempty
    }

-- | A resource-based policy to include in the simulation provided as a string. Each resource in the simulation is treated as if it had this policy attached. You can include only one resource-based policy in a simulation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpResourcePolicy :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe Lude.Text)
scpResourcePolicy = Lens.lens (resourcePolicy :: SimulateCustomPolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourcePolicy = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | The ARN of the IAM user that you want to use as the simulated caller of the API operations. @CallerArn@ is required if you include a @ResourcePolicy@ so that the policy's @Principal@ element has a value to use in evaluating the policy.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
--
-- /Note:/ Consider using 'callerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpCallerARN :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe Lude.Text)
scpCallerARN = Lens.lens (callerARN :: SimulateCustomPolicy -> Lude.Maybe Lude.Text) (\s a -> s {callerARN = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpCallerARN "Use generic-lens or generic-optics with 'callerARN' instead." #-}

-- | Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and security-group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network-interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .
--
--     * __EC2-Classic-InstanceStore__
-- instance, image, security-group
--
--
--     * __EC2-Classic-EBS__
-- instance, image, security-group, volume
--
--
--     * __EC2-VPC-InstanceStore__
-- instance, image, security-group, network-interface
--
--
--     * __EC2-VPC-InstanceStore-Subnet__
-- instance, image, security-group, network-interface, subnet
--
--
--     * __EC2-VPC-EBS__
-- instance, image, security-group, network-interface, volume
--
--
--     * __EC2-VPC-EBS-Subnet__
-- instance, image, security-group, network-interface, subnet, volume
--
--
--
-- /Note:/ Consider using 'resourceHandlingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpResourceHandlingOption :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe Lude.Text)
scpResourceHandlingOption = Lens.lens (resourceHandlingOption :: SimulateCustomPolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceHandlingOption = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpResourceHandlingOption "Use generic-lens or generic-optics with 'resourceHandlingOption' instead." #-}

-- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- If you include a @ResourcePolicy@ , then it must be applicable to all of the resources included in the simulation or you receive an invalid input error.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpResourceARNs :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe [Lude.Text])
scpResourceARNs = Lens.lens (resourceARNs :: SimulateCustomPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceARNs = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that an IAM entity can have. You can input only one permissions boundary when you pass a policy to this operation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string that contains the complete, valid JSON text of a permissions boundary policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'permissionsBoundaryPolicyInputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpPermissionsBoundaryPolicyInputList :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe [Lude.Text])
scpPermissionsBoundaryPolicyInputList = Lens.lens (permissionsBoundaryPolicyInputList :: SimulateCustomPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {permissionsBoundaryPolicyInputList = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpPermissionsBoundaryPolicyInputList "Use generic-lens or generic-optics with 'permissionsBoundaryPolicyInputList' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpMarker :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe Lude.Text)
scpMarker = Lens.lens (marker :: SimulateCustomPolicy -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpMaxItems :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe Lude.Natural)
scpMaxItems = Lens.lens (maxItems :: SimulateCustomPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
--
-- /Note:/ Consider using 'contextEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpContextEntries :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe [ContextEntry])
scpContextEntries = Lens.lens (contextEntries :: SimulateCustomPolicy -> Lude.Maybe [ContextEntry]) (\s a -> s {contextEntries = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpContextEntries "Use generic-lens or generic-optics with 'contextEntries' instead." #-}

-- | An ARN representing the AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- The ARN for an account uses the following syntax: @arn:aws:iam::/AWS-account-ID/ :root@ . For example, to represent the account with the 112233445566 ID, use the following ARN: @arn:aws:iam::112233445566-ID:root@ .
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpResourceOwner :: Lens.Lens' SimulateCustomPolicy (Lude.Maybe Lude.Text)
scpResourceOwner = Lens.lens (resourceOwner :: SimulateCustomPolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceOwner = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpResourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead." #-}

-- | A list of policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. Do not include any resource-based policies in this parameter. Any resource-based policy must be submitted with the @ResourcePolicy@ parameter. The policies cannot be "scope-down" policies, such as you could include in a call to <https://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken> or one of the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole> API operations. In other words, do not use policies designed to restrict what a user can do while using the temporary credentials.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'policyInputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpPolicyInputList :: Lens.Lens' SimulateCustomPolicy [Lude.Text]
scpPolicyInputList = Lens.lens (policyInputList :: SimulateCustomPolicy -> [Lude.Text]) (\s a -> s {policyInputList = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpPolicyInputList "Use generic-lens or generic-optics with 'policyInputList' instead." #-}

-- | A list of names of API operations to evaluate in the simulation. Each operation is evaluated against each resource. Each operation must include the service identifier, such as @iam:CreateUser@ . This operation does not support using wildcards (*) in an action name.
--
-- /Note:/ Consider using 'actionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpActionNames :: Lens.Lens' SimulateCustomPolicy [Lude.Text]
scpActionNames = Lens.lens (actionNames :: SimulateCustomPolicy -> [Lude.Text]) (\s a -> s {actionNames = a} :: SimulateCustomPolicy)
{-# DEPRECATED scpActionNames "Use generic-lens or generic-optics with 'actionNames' instead." #-}

instance Page.AWSPager SimulateCustomPolicy where
  page rq rs
    | Page.stop (rs Lens.^. spIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. spMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& scpMarker Lens..~ rs Lens.^. spMarker

instance Lude.AWSRequest SimulateCustomPolicy where
  type Rs SimulateCustomPolicy = SimulatePolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "SimulateCustomPolicyResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders SimulateCustomPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SimulateCustomPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery SimulateCustomPolicy where
  toQuery SimulateCustomPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SimulateCustomPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "ResourcePolicy" Lude.=: resourcePolicy,
        "CallerArn" Lude.=: callerARN,
        "ResourceHandlingOption" Lude.=: resourceHandlingOption,
        "ResourceArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> resourceARNs),
        "PermissionsBoundaryPolicyInputList"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "member"
                Lude.<$> permissionsBoundaryPolicyInputList
            ),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "ContextEntries"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> contextEntries),
        "ResourceOwner" Lude.=: resourceOwner,
        "PolicyInputList"
          Lude.=: Lude.toQueryList "member" policyInputList,
        "ActionNames" Lude.=: Lude.toQueryList "member" actionNames
      ]
