{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SimulateCustomPolicy (..)
    , mkSimulateCustomPolicy
    -- ** Request lenses
    , scpPolicyInputList
    , scpActionNames
    , scpCallerArn
    , scpContextEntries
    , scpMarker
    , scpMaxItems
    , scpPermissionsBoundaryPolicyInputList
    , scpResourceArns
    , scpResourceHandlingOption
    , scpResourceOwner
    , scpResourcePolicy

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

-- | /See:/ 'mkSimulateCustomPolicy' smart constructor.
data SimulateCustomPolicy = SimulateCustomPolicy'
  { policyInputList :: [Types.PolicyDocumentType]
    -- ^ A list of policy documents to include in the simulation. Each document is specified as a string containing the complete, valid JSON text of an IAM policy. Do not include any resource-based policies in this parameter. Any resource-based policy must be submitted with the @ResourcePolicy@ parameter. The policies cannot be "scope-down" policies, such as you could include in a call to <https://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken> or one of the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole> API operations. In other words, do not use policies designed to restrict what a user can do while using the temporary credentials.
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
  , actionNames :: [Types.ActionNameType]
    -- ^ A list of names of API operations to evaluate in the simulation. Each operation is evaluated against each resource. Each operation must include the service identifier, such as @iam:CreateUser@ . This operation does not support using wildcards (*) in an action name.
  , callerArn :: Core.Maybe Types.CallerArn
    -- ^ The ARN of the IAM user that you want to use as the simulated caller of the API operations. @CallerArn@ is required if you include a @ResourcePolicy@ so that the policy's @Principal@ element has a value to use in evaluating the policy.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
  , contextEntries :: Core.Maybe [Types.ContextEntry]
    -- ^ A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
  , marker :: Core.Maybe Types.Marker
    -- ^ Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
  , permissionsBoundaryPolicyInputList :: Core.Maybe [Types.PolicyDocumentType]
    -- ^ The IAM permissions boundary policy to simulate. The permissions boundary sets the maximum permissions that an IAM entity can have. You can input only one permissions boundary when you pass a policy to this operation. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Entities> in the /IAM User Guide/ . The policy input is specified as a string that contains the complete, valid JSON text of a permissions boundary policy.
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
-- If you include a @ResourcePolicy@ , then it must be applicable to all of the resources included in the simulation or you receive an invalid input error.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , resourceHandlingOption :: Core.Maybe Types.ResourceHandlingOption
    -- ^ Specifies the type of simulation to run. Different API operations that support resource-based policies require different combinations of resources. By specifying the type of simulation to run, you enable the policy simulator to enforce the presence of the required resources to ensure reliable simulation results. If your simulation does not match one of the following scenarios, then you can omit this parameter. The following list shows each of the supported scenario values and the resources that you must define to run the simulation.
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
  , resourceOwner :: Core.Maybe Types.ResourceOwner
    -- ^ An ARN representing the AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- The ARN for an account uses the following syntax: @arn:aws:iam::/AWS-account-ID/ :root@ . For example, to represent the account with the 112233445566 ID, use the following ARN: @arn:aws:iam::112233445566-ID:root@ . 
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

-- | Creates a 'SimulateCustomPolicy' value with any optional fields omitted.
mkSimulateCustomPolicy
    :: SimulateCustomPolicy
mkSimulateCustomPolicy
  = SimulateCustomPolicy'{policyInputList = Core.mempty,
                          actionNames = Core.mempty, callerArn = Core.Nothing,
                          contextEntries = Core.Nothing, marker = Core.Nothing,
                          maxItems = Core.Nothing,
                          permissionsBoundaryPolicyInputList = Core.Nothing,
                          resourceArns = Core.Nothing, resourceHandlingOption = Core.Nothing,
                          resourceOwner = Core.Nothing, resourcePolicy = Core.Nothing}

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
scpPolicyInputList :: Lens.Lens' SimulateCustomPolicy [Types.PolicyDocumentType]
scpPolicyInputList = Lens.field @"policyInputList"
{-# INLINEABLE scpPolicyInputList #-}
{-# DEPRECATED policyInputList "Use generic-lens or generic-optics with 'policyInputList' instead"  #-}

-- | A list of names of API operations to evaluate in the simulation. Each operation is evaluated against each resource. Each operation must include the service identifier, such as @iam:CreateUser@ . This operation does not support using wildcards (*) in an action name.
--
-- /Note:/ Consider using 'actionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpActionNames :: Lens.Lens' SimulateCustomPolicy [Types.ActionNameType]
scpActionNames = Lens.field @"actionNames"
{-# INLINEABLE scpActionNames #-}
{-# DEPRECATED actionNames "Use generic-lens or generic-optics with 'actionNames' instead"  #-}

-- | The ARN of the IAM user that you want to use as the simulated caller of the API operations. @CallerArn@ is required if you include a @ResourcePolicy@ so that the policy's @Principal@ element has a value to use in evaluating the policy.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN of an assumed role, federated user, or a service principal.
--
-- /Note:/ Consider using 'callerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpCallerArn :: Lens.Lens' SimulateCustomPolicy (Core.Maybe Types.CallerArn)
scpCallerArn = Lens.field @"callerArn"
{-# INLINEABLE scpCallerArn #-}
{-# DEPRECATED callerArn "Use generic-lens or generic-optics with 'callerArn' instead"  #-}

-- | A list of context keys and corresponding values for the simulation to use. Whenever a context key is evaluated in one of the simulated IAM permissions policies, the corresponding value is supplied.
--
-- /Note:/ Consider using 'contextEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpContextEntries :: Lens.Lens' SimulateCustomPolicy (Core.Maybe [Types.ContextEntry])
scpContextEntries = Lens.field @"contextEntries"
{-# INLINEABLE scpContextEntries #-}
{-# DEPRECATED contextEntries "Use generic-lens or generic-optics with 'contextEntries' instead"  #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpMarker :: Lens.Lens' SimulateCustomPolicy (Core.Maybe Types.Marker)
scpMarker = Lens.field @"marker"
{-# INLINEABLE scpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpMaxItems :: Lens.Lens' SimulateCustomPolicy (Core.Maybe Core.Natural)
scpMaxItems = Lens.field @"maxItems"
{-# INLINEABLE scpMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

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
scpPermissionsBoundaryPolicyInputList :: Lens.Lens' SimulateCustomPolicy (Core.Maybe [Types.PolicyDocumentType])
scpPermissionsBoundaryPolicyInputList = Lens.field @"permissionsBoundaryPolicyInputList"
{-# INLINEABLE scpPermissionsBoundaryPolicyInputList #-}
{-# DEPRECATED permissionsBoundaryPolicyInputList "Use generic-lens or generic-optics with 'permissionsBoundaryPolicyInputList' instead"  #-}

-- | A list of ARNs of AWS resources to include in the simulation. If this parameter is not provided, then the value defaults to @*@ (all resources). Each API in the @ActionNames@ parameter is evaluated for each resource in this list. The simulation determines the access result (allowed or denied) of each combination and reports it in the response.
--
-- The simulation does not automatically retrieve policies for the specified resources. If you want to include a resource policy in the simulation, then you must include the policy as a string in the @ResourcePolicy@ parameter.
-- If you include a @ResourcePolicy@ , then it must be applicable to all of the resources included in the simulation or you receive an invalid input error.
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpResourceArns :: Lens.Lens' SimulateCustomPolicy (Core.Maybe [Types.ResourceNameType])
scpResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE scpResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

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
scpResourceHandlingOption :: Lens.Lens' SimulateCustomPolicy (Core.Maybe Types.ResourceHandlingOption)
scpResourceHandlingOption = Lens.field @"resourceHandlingOption"
{-# INLINEABLE scpResourceHandlingOption #-}
{-# DEPRECATED resourceHandlingOption "Use generic-lens or generic-optics with 'resourceHandlingOption' instead"  #-}

-- | An ARN representing the AWS account ID that specifies the owner of any simulated resource that does not identify its owner in the resource ARN. Examples of resource ARNs include an S3 bucket or object. If @ResourceOwner@ is specified, it is also used as the account owner of any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@ parameter is not specified, then the owner of the resources and the resource policy defaults to the account of the identity provided in @CallerArn@ . This parameter is required only if you specify a resource-based policy and account that owns the resource is different from the account that owns the simulated calling user @CallerArn@ .
--
-- The ARN for an account uses the following syntax: @arn:aws:iam::/AWS-account-ID/ :root@ . For example, to represent the account with the 112233445566 ID, use the following ARN: @arn:aws:iam::112233445566-ID:root@ . 
--
-- /Note:/ Consider using 'resourceOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scpResourceOwner :: Lens.Lens' SimulateCustomPolicy (Core.Maybe Types.ResourceOwner)
scpResourceOwner = Lens.field @"resourceOwner"
{-# INLINEABLE scpResourceOwner #-}
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
scpResourcePolicy :: Lens.Lens' SimulateCustomPolicy (Core.Maybe Types.ResourcePolicy)
scpResourcePolicy = Lens.field @"resourcePolicy"
{-# INLINEABLE scpResourcePolicy #-}
{-# DEPRECATED resourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead"  #-}

instance Core.ToQuery SimulateCustomPolicy where
        toQuery SimulateCustomPolicy{..}
          = Core.toQueryPair "Action" ("SimulateCustomPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "PolicyInputList"
                (Core.toQueryList "member" policyInputList)
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

instance Core.ToHeaders SimulateCustomPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SimulateCustomPolicy where
        type Rs SimulateCustomPolicy = Types.SimulatePolicyResponse
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
          = Response.receiveXMLWrapper "SimulateCustomPolicyResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SimulateCustomPolicy where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")
