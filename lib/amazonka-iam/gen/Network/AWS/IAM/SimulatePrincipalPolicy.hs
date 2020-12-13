{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SimulatePrincipalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate how a set of IAM policies attached to an IAM entity works with a list of API operations and AWS resources to determine the policies' effective permissions. The entity can be an IAM user, group, or role. If you specify a user, then the simulation also includes all of the policies that are attached to groups that the user belongs to.
--
-- You can optionally include a list of one or more additional policies specified as strings to include in the simulation. If you want to simulate only policies specified as strings, use 'SimulateCustomPolicy' instead.
-- You can also optionally include one resource-based policy to be evaluated with each of the resources included in the simulation.
-- The simulation does not perform the API operations; it only checks the authorization to determine if the simulated policies allow or deny the operations.
-- __Note:__ This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use 'SimulateCustomPolicy' instead.
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the @Condition@ element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use 'GetContextKeysForPrincipalPolicy' .
-- If the output is long, you can use the @MaxItems@ and @Marker@ parameters to paginate the results.
--
-- This operation returns paginated results.
module Network.AWS.IAM.SimulatePrincipalPolicy
  ( -- * Creating a request
    SimulatePrincipalPolicy (..),
    mkSimulatePrincipalPolicy,

    -- ** Request lenses
    sppPolicyInputList,
    sppResourcePolicy,
    sppActionNames,
    sppCallerARN,
    sppResourceHandlingOption,
    sppResourceARNs,
    sppPermissionsBoundaryPolicyInputList,
    sppMarker,
    sppMaxItems,
    sppContextEntries,
    sppPolicySourceARN,
    sppResourceOwner,

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

-- | /See:/ 'mkSimulatePrincipalPolicy' smart constructor.
data SimulatePrincipalPolicy = SimulatePrincipalPolicy'
  { -- | An optional list of additional policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
    policyInputList :: Lude.Maybe [Lude.Text],
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
    resourcePolicy :: Lude.Maybe Lude.Text,
    -- | A list of names of API operations to evaluate in the simulation. Each operation is evaluated for each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
    actionNames :: [Lude.Text],
    -- | The ARN of the IAM user that you want to specify as the simulated caller of the API operations. If you do not specify a @CallerArn@ , it defaults to the ARN of the user that you specify in @PolicySourceArn@ , if you specified a user. If you include both a @PolicySourceArn@ (for example, @arn:aws:iam::123456789012:user/David@ ) and a @CallerArn@ (for example, @arn:aws:iam::123456789012:user/Bob@ ), the result is that you simulate calling the API operations as Bob, as if Bob had David's policies.
    --
    -- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
    -- @CallerArn@ is required if you include a @ResourcePolicy@ and the @PolicySourceArn@ is not the ARN for an IAM user. This is required so that the resource-based policy's @Principal@ element has a value to use in evaluating the policy.
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    callerARN :: Lude.Maybe Lude.Text,
    -- | Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
    --
    -- Each of the EC2 scenarios requires that you specify instance, image, and security group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .
    --
    --     * __EC2-Classic-InstanceStore__
    -- instance, image, security group
    --
    --
    --     * __EC2-Classic-EBS__
    -- instance, image, security group, volume
    --
    --
    --     * __EC2-VPC-InstanceStore__
    -- instance, image, security group, network interface
    --
    --
    --     * __EC2-VPC-InstanceStore-Subnet__
    -- instance, image, security group, network interface, subnet
    --
    --
    --     * __EC2-VPC-EBS__
    -- instance, image, security group, network interface, volume
    --
    --
    --     * __EC2-VPC-EBS-Subnet__
    -- instance, image, security group, network interface, subnet, volume
    resourceHandlingOption :: Lude.Maybe Lude.Text,
    -- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
    --
    -- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    resourceARNs :: Lude.Maybe [Lude.Text],
    -- | The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that the entity can have. You can input only one permissions boundary when you pass a policy to this operation. An IAM entity can only have one permissions boundary in effect at a time. For example, if a permissions boundary is attached to an entity and you pass in a different permissions boundary policy using this parameter, then the new permissions boundary policy is used for the simulation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string containing the complete, valid JSON text of a permissions boundary policy.
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
    permissionsBoundaryPolicyInputList :: Lude.Maybe [Lude.Text],
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
    contextEntries :: Lude.Maybe [ContextEntry],
    -- | The Amazon Resource Name (ARN) of a user, group, or role whose policies you want to include in the simulation. If you specify a user, group, or role, the simulation includes all policies that are associated with that entity. If you specify a user, the simulation also includes all policies that are attached to any groups the user belongs to.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policySourceARN :: Lude.Text,
    -- | An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
    resourceOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimulatePrincipalPolicy' with the minimum fields required to make a request.
--
-- * 'policyInputList' - An optional list of additional policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
--
--
-- * 'actionNames' - A list of names of API operations to evaluate in the simulation. Each operation is evaluated for each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
-- * 'callerARN' - The ARN of the IAM user that you want to specify as the simulated caller of the API operations. If you do not specify a @CallerArn@ , it defaults to the ARN of the user that you specify in @PolicySourceArn@ , if you specified a user. If you include both a @PolicySourceArn@ (for example, @arn:aws:iam::123456789012:user/David@ ) and a @CallerArn@ (for example, @arn:aws:iam::123456789012:user/Bob@ ), the result is that you simulate calling the API operations as Bob, as if Bob had David's policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
-- @CallerArn@ is required if you include a @ResourcePolicy@ and the @PolicySourceArn@ is not the ARN for an IAM user. This is required so that the resource-based policy's @Principal@ element has a value to use in evaluating the policy.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'resourceHandlingOption' - Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and security group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .
--
--     * __EC2-Classic-InstanceStore__
-- instance, image, security group
--
--
--     * __EC2-Classic-EBS__
-- instance, image, security group, volume
--
--
--     * __EC2-VPC-InstanceStore__
-- instance, image, security group, network interface
--
--
--     * __EC2-VPC-InstanceStore-Subnet__
-- instance, image, security group, network interface, subnet
--
--
--     * __EC2-VPC-EBS__
-- instance, image, security group, network interface, volume
--
--
--     * __EC2-VPC-EBS-Subnet__
-- instance, image, security group, network interface, subnet, volume
--
--
-- * 'resourceARNs' - A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'permissionsBoundaryPolicyInputList' - The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that the entity can have. You can input only one permissions boundary when you pass a policy to this operation. An IAM entity can only have one permissions boundary in effect at a time. For example, if a permissions boundary is attached to an entity and you pass in a different permissions boundary policy using this parameter, then the new permissions boundary policy is used for the simulation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string containing the complete, valid JSON text of a permissions boundary policy.
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
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'contextEntries' - A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
-- * 'policySourceARN' - The Amazon Resource Name (ARN) of a user, group, or role whose policies you want to include in the simulation. If you specify a user, group, or role, the simulation includes all policies that are associated with that entity. If you specify a user, the simulation also includes all policies that are attached to any groups the user belongs to.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'resourceOwner' - An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
mkSimulatePrincipalPolicy ::
  -- | 'policySourceARN'
  Lude.Text ->
  SimulatePrincipalPolicy
mkSimulatePrincipalPolicy pPolicySourceARN_ =
  SimulatePrincipalPolicy'
    { policyInputList = Lude.Nothing,
      resourcePolicy = Lude.Nothing,
      actionNames = Lude.mempty,
      callerARN = Lude.Nothing,
      resourceHandlingOption = Lude.Nothing,
      resourceARNs = Lude.Nothing,
      permissionsBoundaryPolicyInputList = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      contextEntries = Lude.Nothing,
      policySourceARN = pPolicySourceARN_,
      resourceOwner = Lude.Nothing
    }

-- | An optional list of additional policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
sppPolicyInputList :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe [Lude.Text])
sppPolicyInputList = Lens.lens (policyInputList :: SimulatePrincipalPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {policyInputList = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppPolicyInputList "Use generic-lens or generic-optics with 'policyInputList' instead." #-}

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
sppResourcePolicy :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe Lude.Text)
sppResourcePolicy = Lens.lens (resourcePolicy :: SimulatePrincipalPolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourcePolicy = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | A list of names of API operations to evaluate in the simulation. Each operation is evaluated for each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
--
-- /Note:/ Consider using 'actionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppActionNames :: Lens.Lens' SimulatePrincipalPolicy [Lude.Text]
sppActionNames = Lens.lens (actionNames :: SimulatePrincipalPolicy -> [Lude.Text]) (\s a -> s {actionNames = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppActionNames "Use generic-lens or generic-optics with 'actionNames' instead." #-}

-- | The ARN of the IAM user that you want to specify as the simulated caller of the API operations. If you do not specify a @CallerArn@ , it defaults to the ARN of the user that you specify in @PolicySourceArn@ , if you specified a user. If you include both a @PolicySourceArn@ (for example, @arn:aws:iam::123456789012:user/David@ ) and a @CallerArn@ (for example, @arn:aws:iam::123456789012:user/Bob@ ), the result is that you simulate calling the API operations as Bob, as if Bob had David's policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
-- @CallerArn@ is required if you include a @ResourcePolicy@ and the @PolicySourceArn@ is not the ARN for an IAM user. This is required so that the resource-based policy's @Principal@ element has a value to use in evaluating the policy.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'callerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppCallerARN :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe Lude.Text)
sppCallerARN = Lens.lens (callerARN :: SimulatePrincipalPolicy -> Lude.Maybe Lude.Text) (\s a -> s {callerARN = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppCallerARN "Use generic-lens or generic-optics with 'callerARN' instead." #-}

-- | Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and security group resources. If your scenario includes an EBS volume, then you must specify that volume as a resource. If the EC2 scenario includes VPC, then you must supply the network interface resource. If it includes an IP subnet, then you must specify the subnet resource. For more information on the EC2 scenario options, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported Platforms> in the /Amazon EC2 User Guide/ .
--
--     * __EC2-Classic-InstanceStore__
-- instance, image, security group
--
--
--     * __EC2-Classic-EBS__
-- instance, image, security group, volume
--
--
--     * __EC2-VPC-InstanceStore__
-- instance, image, security group, network interface
--
--
--     * __EC2-VPC-InstanceStore-Subnet__
-- instance, image, security group, network interface, subnet
--
--
--     * __EC2-VPC-EBS__
-- instance, image, security group, network interface, volume
--
--
--     * __EC2-VPC-EBS-Subnet__
-- instance, image, security group, network interface, subnet, volume
--
--
--
-- /Note:/ Consider using 'resourceHandlingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppResourceHandlingOption :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe Lude.Text)
sppResourceHandlingOption = Lens.lens (resourceHandlingOption :: SimulatePrincipalPolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceHandlingOption = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppResourceHandlingOption "Use generic-lens or generic-optics with 'resourceHandlingOption' instead." #-}

-- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppResourceARNs :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe [Lude.Text])
sppResourceARNs = Lens.lens (resourceARNs :: SimulatePrincipalPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceARNs = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that the entity can have. You can input only one permissions boundary when you pass a policy to this operation. An IAM entity can only have one permissions boundary in effect at a time. For example, if a permissions boundary is attached to an entity and you pass in a different permissions boundary policy using this parameter, then the new permissions boundary policy is used for the simulation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string containing the complete, valid JSON text of a permissions boundary policy.
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
sppPermissionsBoundaryPolicyInputList :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe [Lude.Text])
sppPermissionsBoundaryPolicyInputList = Lens.lens (permissionsBoundaryPolicyInputList :: SimulatePrincipalPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {permissionsBoundaryPolicyInputList = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppPermissionsBoundaryPolicyInputList "Use generic-lens or generic-optics with 'permissionsBoundaryPolicyInputList' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppMarker :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe Lude.Text)
sppMarker = Lens.lens (marker :: SimulatePrincipalPolicy -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppMaxItems :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe Lude.Natural)
sppMaxItems = Lens.lens (maxItems :: SimulatePrincipalPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
--
-- /Note:/ Consider using 'contextEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppContextEntries :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe [ContextEntry])
sppContextEntries = Lens.lens (contextEntries :: SimulatePrincipalPolicy -> Lude.Maybe [ContextEntry]) (\s a -> s {contextEntries = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppContextEntries "Use generic-lens or generic-optics with 'contextEntries' instead." #-}

-- | The Amazon Resource Name (ARN) of a user, group, or role whose policies you want to include in the simulation. If you specify a user, group, or role, the simulation includes all policies that are associated with that entity. If you specify a user, the simulation also includes all policies that are attached to any groups the user belongs to.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policySourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPolicySourceARN :: Lens.Lens' SimulatePrincipalPolicy Lude.Text
sppPolicySourceARN = Lens.lens (policySourceARN :: SimulatePrincipalPolicy -> Lude.Text) (\s a -> s {policySourceARN = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppPolicySourceARN "Use generic-lens or generic-optics with 'policySourceARN' instead." #-}

-- | An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppResourceOwner :: Lens.Lens' SimulatePrincipalPolicy (Lude.Maybe Lude.Text)
sppResourceOwner = Lens.lens (resourceOwner :: SimulatePrincipalPolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceOwner = a} :: SimulatePrincipalPolicy)
{-# DEPRECATED sppResourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead." #-}

instance Page.AWSPager SimulatePrincipalPolicy where
  page rq rs
    | Page.stop (rs Lens.^. spIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. spMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& sppMarker Lens..~ rs Lens.^. spMarker

instance Lude.AWSRequest SimulatePrincipalPolicy where
  type Rs SimulatePrincipalPolicy = SimulatePolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "SimulatePrincipalPolicyResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders SimulatePrincipalPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SimulatePrincipalPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery SimulatePrincipalPolicy where
  toQuery SimulatePrincipalPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SimulatePrincipalPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyInputList"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyInputList),
        "ResourcePolicy" Lude.=: resourcePolicy,
        "ActionNames" Lude.=: Lude.toQueryList "member" actionNames,
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
        "PolicySourceArn" Lude.=: policySourceARN,
        "ResourceOwner" Lude.=: resourceOwner
      ]
