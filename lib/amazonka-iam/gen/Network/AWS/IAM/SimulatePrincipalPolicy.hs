{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SimulatePrincipalPolicy (..)
    , mkSimulatePrincipalPolicy
    -- ** Request lenses
    , sppPolicySourceArn
    , sppActionNames
    , sppCallerArn
    , sppContextEntries
    , sppMarker
    , sppMaxItems
    , sppPermissionsBoundaryPolicyInputList
    , sppPolicyInputList
    , sppResourceArns
    , sppResourceHandlingOption
    , sppResourceOwner
    , sppResourcePolicy

     -- * Destructuring the response
    , Types.SimulatePolicyResponse (..)
    , Types.mkSimulatePolicyResponse
    -- ** Response lenses
    , Types.sprEvaluationResults
    , Types.sprIsTruncated
    , Types.sprMarker
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSimulatePrincipalPolicy' smart constructor.
data SimulatePrincipalPolicy = SimulatePrincipalPolicy'
  { policySourceArn :: Types.PolicySourceArn
    -- ^ The Amazon Resource Name (ARN) of a user, group, or role whose policies you want to include in the simulation. If you specify a user, group, or role, the simulation includes all policies that are associated with that entity. If you specify a user, the simulation also includes all policies that are attached to any groups the user belongs to.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , actionNames :: [Types.ActionNameType]
    -- ^ A list of names of API operations to evaluate in the simulation. Each operation is evaluated for each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
  , callerArn :: Core.Maybe Types.CallerArn
    -- ^ The ARN of the IAM user that you want to specify as the simulated caller of the API operations. If you do not specify a @CallerArn@ , it defaults to the ARN of the user that you specify in @PolicySourceArn@ , if you specified a user. If you include both a @PolicySourceArn@ (for example, @arn:aws:iam::123456789012:user/David@ ) and a @CallerArn@ (for example, @arn:aws:iam::123456789012:user/Bob@ ), the result is that you simulate calling the API operations as Bob, as if Bob had David's policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
-- @CallerArn@ is required if you include a @ResourcePolicy@ and the @PolicySourceArn@ is not the ARN for an IAM user. This is required so that the resource-based policy's @Principal@ element has a value to use in evaluating the policy.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , contextEntries :: Core.Maybe [Types.ContextEntry]
    -- ^ A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
  , marker :: Core.Maybe Types.MarkerType
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  , permissionsBoundaryPolicyInputList :: Core.Maybe [Types.PolicyDocumentType]
    -- ^ The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that the entity can have. You can input only one permissions boundary when you pass a policy to this operation. An IAM entity can only have one permissions boundary in effect at a time. For example, if a permissions boundary is attached to an entity and you pass in a different permissions boundary policy using this parameter, then the new permissions boundary policy is used for the simulation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string containing the complete, valid JSON text of a permissions boundary policy.
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
  , policyInputList :: Core.Maybe [Types.PolicyDocumentType]
    -- ^ An optional list of additional policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
  , resourceArns :: Core.Maybe [Types.ResourceNameType]
    -- ^ A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , resourceHandlingOption :: Core.Maybe Types.ResourceHandlingOption
    -- ^ Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
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
  , resourceOwner :: Core.Maybe Types.ResourceOwner
    -- ^ An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
  , resourcePolicy :: Core.Maybe Types.ResourcePolicy
    -- ^ A resource-based policy to include in the simulation provided as a string. Each resource in the simulation is treated as if it had this policy attached. You can include only one resource-based policy in a simulation.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SimulatePrincipalPolicy' value with any optional fields omitted.
mkSimulatePrincipalPolicy
    :: Types.PolicySourceArn -- ^ 'policySourceArn'
    -> SimulatePrincipalPolicy
mkSimulatePrincipalPolicy policySourceArn
  = SimulatePrincipalPolicy'{policySourceArn,
                             actionNames = Core.mempty, callerArn = Core.Nothing,
                             contextEntries = Core.Nothing, marker = Core.Nothing,
                             maxItems = Core.Nothing,
                             permissionsBoundaryPolicyInputList = Core.Nothing,
                             policyInputList = Core.Nothing, resourceArns = Core.Nothing,
                             resourceHandlingOption = Core.Nothing,
                             resourceOwner = Core.Nothing, resourcePolicy = Core.Nothing}

-- | The Amazon Resource Name (ARN) of a user, group, or role whose policies you want to include in the simulation. If you specify a user, group, or role, the simulation includes all policies that are associated with that entity. If you specify a user, the simulation also includes all policies that are attached to any groups the user belongs to.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policySourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPolicySourceArn :: Lens.Lens' SimulatePrincipalPolicy Types.PolicySourceArn
sppPolicySourceArn = Lens.field @"policySourceArn"
{-# INLINEABLE sppPolicySourceArn #-}
{-# DEPRECATED policySourceArn "Use generic-lens or generic-optics with 'policySourceArn' instead"  #-}

-- | A list of names of API operations to evaluate in the simulation. Each operation is evaluated for each resource. Each operation must include the service identifier, such as @iam:CreateUser@ .
--
-- /Note:/ Consider using 'actionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppActionNames :: Lens.Lens' SimulatePrincipalPolicy [Types.ActionNameType]
sppActionNames = Lens.field @"actionNames"
{-# INLINEABLE sppActionNames #-}
{-# DEPRECATED actionNames "Use generic-lens or generic-optics with 'actionNames' instead"  #-}

-- | The ARN of the IAM user that you want to specify as the simulated caller of the API operations. If you do not specify a @CallerArn@ , it defaults to the ARN of the user that you specify in @PolicySourceArn@ , if you specified a user. If you include both a @PolicySourceArn@ (for example, @arn:aws:iam::123456789012:user/David@ ) and a @CallerArn@ (for example, @arn:aws:iam::123456789012:user/Bob@ ), the result is that you simulate calling the API operations as Bob, as if Bob had David's policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
-- @CallerArn@ is required if you include a @ResourcePolicy@ and the @PolicySourceArn@ is not the ARN for an IAM user. This is required so that the resource-based policy's @Principal@ element has a value to use in evaluating the policy.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'callerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppCallerArn :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe Types.CallerArn)
sppCallerArn = Lens.field @"callerArn"
{-# INLINEABLE sppCallerArn #-}
{-# DEPRECATED callerArn "Use generic-lens or generic-optics with 'callerArn' instead"  #-}

-- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
--
-- /Note:/ Consider using 'contextEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppContextEntries :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe [Types.ContextEntry])
sppContextEntries = Lens.field @"contextEntries"
{-# INLINEABLE sppContextEntries #-}
{-# DEPRECATED contextEntries "Use generic-lens or generic-optics with 'contextEntries' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppMarker :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe Types.MarkerType)
sppMarker = Lens.field @"marker"
{-# INLINEABLE sppMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppMaxItems :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe Core.Natural)
sppMaxItems = Lens.field @"maxItems"
{-# INLINEABLE sppMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

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
sppPermissionsBoundaryPolicyInputList :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe [Types.PolicyDocumentType])
sppPermissionsBoundaryPolicyInputList = Lens.field @"permissionsBoundaryPolicyInputList"
{-# INLINEABLE sppPermissionsBoundaryPolicyInputList #-}
{-# DEPRECATED permissionsBoundaryPolicyInputList "Use generic-lens or generic-optics with 'permissionsBoundaryPolicyInputList' instead"  #-}

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
sppPolicyInputList :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe [Types.PolicyDocumentType])
sppPolicyInputList = Lens.field @"policyInputList"
{-# INLINEABLE sppPolicyInputList #-}
{-# DEPRECATED policyInputList "Use generic-lens or generic-optics with 'policyInputList' instead"  #-}

-- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppResourceArns :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe [Types.ResourceNameType])
sppResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE sppResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

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
sppResourceHandlingOption :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe Types.ResourceHandlingOption)
sppResourceHandlingOption = Lens.field @"resourceHandlingOption"
{-# INLINEABLE sppResourceHandlingOption #-}
{-# DEPRECATED resourceHandlingOption "Use generic-lens or generic-optics with 'resourceHandlingOption' instead"  #-}

-- | An AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppResourceOwner :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe Types.ResourceOwner)
sppResourceOwner = Lens.field @"resourceOwner"
{-# INLINEABLE sppResourceOwner #-}
{-# DEPRECATED resourceOwner "Use generic-lens or generic-optics with 'resourceOwner' instead"  #-}

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
sppResourcePolicy :: Lens.Lens' SimulatePrincipalPolicy (Core.Maybe Types.ResourcePolicy)
sppResourcePolicy = Lens.field @"resourcePolicy"
{-# INLINEABLE sppResourcePolicy #-}
{-# DEPRECATED resourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead"  #-}

instance Core.ToQuery SimulatePrincipalPolicy where
        toQuery SimulatePrincipalPolicy{..}
          = Core.toQueryPair "Action"
              ("SimulatePrincipalPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicySourceArn" policySourceArn
              Core.<>
              Core.toQueryPair "ActionNames"
                (Core.toQueryList "member" actionNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CallerArn") callerArn
              Core.<>
              Core.toQueryPair "ContextEntries"
                (Core.maybe Core.mempty (Core.toQueryList "member") contextEntries)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<>
              Core.toQueryPair "PermissionsBoundaryPolicyInputList"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   permissionsBoundaryPolicyInputList)
              Core.<>
              Core.toQueryPair "PolicyInputList"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   policyInputList)
              Core.<>
              Core.toQueryPair "ResourceArns"
                (Core.maybe Core.mempty (Core.toQueryList "member") resourceArns)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResourceHandlingOption")
                resourceHandlingOption
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResourceOwner")
                resourceOwner
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ResourcePolicy")
                resourcePolicy

instance Core.ToHeaders SimulatePrincipalPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SimulatePrincipalPolicy where
        type Rs SimulatePrincipalPolicy = Types.SimulatePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "SimulatePrincipalPolicyResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SimulatePrincipalPolicy where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")
