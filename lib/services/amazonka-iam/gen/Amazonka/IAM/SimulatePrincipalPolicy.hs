{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IAM.SimulatePrincipalPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate how a set of IAM policies attached to an IAM entity works with
-- a list of API operations and Amazon Web Services resources to determine
-- the policies\' effective permissions. The entity can be an IAM user,
-- group, or role. If you specify a user, then the simulation also includes
-- all of the policies that are attached to groups that the user belongs
-- to. You can simulate resources that don\'t exist in your account.
--
-- You can optionally include a list of one or more additional policies
-- specified as strings to include in the simulation. If you want to
-- simulate only policies specified as strings, use SimulateCustomPolicy
-- instead.
--
-- You can also optionally include one resource-based policy to be
-- evaluated with each of the resources included in the simulation.
--
-- The simulation does not perform the API operations; it only checks the
-- authorization to determine if the simulated policies allow or deny the
-- operations.
--
-- __Note:__ This operation discloses information about the permissions
-- granted to other users. If you do not want users to see other user\'s
-- permissions, then consider allowing them to use SimulateCustomPolicy
-- instead.
--
-- Context keys are variables maintained by Amazon Web Services and its
-- services that provide details about the context of an API query request.
-- You can use the @Condition@ element of an IAM policy to evaluate context
-- keys. To get the list of context keys that the policies require for
-- correct simulation, use GetContextKeysForPrincipalPolicy.
--
-- If the output is long, you can use the @MaxItems@ and @Marker@
-- parameters to paginate the results.
--
-- For more information about using the policy simulator, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_testing-policies.html Testing IAM policies with the IAM policy simulator>
-- in the /IAM User Guide/.
--
-- This operation returns paginated results.
module Amazonka.IAM.SimulatePrincipalPolicy
  ( -- * Creating a Request
    SimulatePrincipalPolicy (..),
    newSimulatePrincipalPolicy,

    -- * Request Lenses
    simulatePrincipalPolicy_callerArn,
    simulatePrincipalPolicy_contextEntries,
    simulatePrincipalPolicy_marker,
    simulatePrincipalPolicy_maxItems,
    simulatePrincipalPolicy_permissionsBoundaryPolicyInputList,
    simulatePrincipalPolicy_policyInputList,
    simulatePrincipalPolicy_resourceArns,
    simulatePrincipalPolicy_resourceHandlingOption,
    simulatePrincipalPolicy_resourceOwner,
    simulatePrincipalPolicy_resourcePolicy,
    simulatePrincipalPolicy_policySourceArn,
    simulatePrincipalPolicy_actionNames,

    -- * Destructuring the Response
    SimulatePolicyResponse (..),
    newSimulatePolicyResponse,

    -- * Response Lenses
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_isTruncated,
    simulatePolicyResponse_marker,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSimulatePrincipalPolicy' smart constructor.
data SimulatePrincipalPolicy = SimulatePrincipalPolicy'
  { -- | The ARN of the IAM user that you want to specify as the simulated caller
    -- of the API operations. If you do not specify a @CallerArn@, it defaults
    -- to the ARN of the user that you specify in @PolicySourceArn@, if you
    -- specified a user. If you include both a @PolicySourceArn@ (for example,
    -- @arn:aws:iam::123456789012:user\/David@) and a @CallerArn@ (for example,
    -- @arn:aws:iam::123456789012:user\/Bob@), the result is that you simulate
    -- calling the API operations as Bob, as if Bob had David\'s policies.
    --
    -- You can specify only the ARN of an IAM user. You cannot specify the ARN
    -- of an assumed role, federated user, or a service principal.
    --
    -- @CallerArn@ is required if you include a @ResourcePolicy@ and the
    -- @PolicySourceArn@ is not the ARN for an IAM user. This is required so
    -- that the resource-based policy\'s @Principal@ element has a value to use
    -- in evaluating the policy.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    callerArn :: Prelude.Maybe Prelude.Text,
    -- | A list of context keys and corresponding values for the simulation to
    -- use. Whenever a context key is evaluated in one of the simulated IAM
    -- permissions policies, the corresponding value is supplied.
    contextEntries :: Prelude.Maybe [ContextEntry],
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The IAM permissions boundary policy to simulate. The permissions
    -- boundary sets the maximum permissions that the entity can have. You can
    -- input only one permissions boundary when you pass a policy to this
    -- operation. An IAM entity can only have one permissions boundary in
    -- effect at a time. For example, if a permissions boundary is attached to
    -- an entity and you pass in a different permissions boundary policy using
    -- this parameter, then the new permissions boundary policy is used for the
    -- simulation. For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
    -- in the /IAM User Guide/. The policy input is specified as a string
    -- containing the complete, valid JSON text of a permissions boundary
    -- policy.
    --
    -- The maximum length of the policy document that you can pass in this
    -- operation, including whitespace, is listed below. To view the maximum
    -- character counts of a managed policy with no whitespaces, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    permissionsBoundaryPolicyInputList :: Prelude.Maybe [Prelude.Text],
    -- | An optional list of additional policy documents to include in the
    -- simulation. Each document is specified as a string containing the
    -- complete, valid JSON text of an IAM policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    policyInputList :: Prelude.Maybe [Prelude.Text],
    -- | A list of ARNs of Amazon Web Services resources to include in the
    -- simulation. If this parameter is not provided, then the value defaults
    -- to @*@ (all resources). Each API in the @ActionNames@ parameter is
    -- evaluated for each resource in this list. The simulation determines the
    -- access result (allowed or denied) of each combination and reports it in
    -- the response. You can simulate resources that don\'t exist in your
    -- account.
    --
    -- The simulation does not automatically retrieve policies for the
    -- specified resources. If you want to include a resource policy in the
    -- simulation, then you must include the policy as a string in the
    -- @ResourcePolicy@ parameter.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the type of simulation to run. Different API operations that
    -- support resource-based policies require different combinations of
    -- resources. By specifying the type of simulation to run, you enable the
    -- policy simulator to enforce the presence of the required resources to
    -- ensure reliable simulation results. If your simulation does not match
    -- one of the following scenarios, then you can omit this parameter. The
    -- following list shows each of the supported scenario values and the
    -- resources that you must define to run the simulation.
    --
    -- Each of the EC2 scenarios requires that you specify instance, image, and
    -- security group resources. If your scenario includes an EBS volume, then
    -- you must specify that volume as a resource. If the EC2 scenario includes
    -- VPC, then you must supply the network interface resource. If it includes
    -- an IP subnet, then you must specify the subnet resource. For more
    -- information on the EC2 scenario options, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported platforms>
    -- in the /Amazon EC2 User Guide/.
    --
    -- -   __EC2-VPC-InstanceStore__
    --
    --     instance, image, security group, network interface
    --
    -- -   __EC2-VPC-InstanceStore-Subnet__
    --
    --     instance, image, security group, network interface, subnet
    --
    -- -   __EC2-VPC-EBS__
    --
    --     instance, image, security group, network interface, volume
    --
    -- -   __EC2-VPC-EBS-Subnet__
    --
    --     instance, image, security group, network interface, subnet, volume
    resourceHandlingOption :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Web Services account ID that specifies the owner of any
    -- simulated resource that does not identify its owner in the resource ARN.
    -- Examples of resource ARNs include an S3 bucket or object. If
    -- @ResourceOwner@ is specified, it is also used as the account owner of
    -- any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@
    -- parameter is not specified, then the owner of the resources and the
    -- resource policy defaults to the account of the identity provided in
    -- @CallerArn@. This parameter is required only if you specify a
    -- resource-based policy and account that owns the resource is different
    -- from the account that owns the simulated calling user @CallerArn@.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | A resource-based policy to include in the simulation provided as a
    -- string. Each resource in the simulation is treated as if it had this
    -- policy attached. You can include only one resource-based policy in a
    -- simulation.
    --
    -- The maximum length of the policy document that you can pass in this
    -- operation, including whitespace, is listed below. To view the maximum
    -- character counts of a managed policy with no whitespaces, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a user, group, or role whose policies
    -- you want to include in the simulation. If you specify a user, group, or
    -- role, the simulation includes all policies that are associated with that
    -- entity. If you specify a user, the simulation also includes all policies
    -- that are attached to any groups the user belongs to.
    --
    -- The maximum length of the policy document that you can pass in this
    -- operation, including whitespace, is listed below. To view the maximum
    -- character counts of a managed policy with no whitespaces, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policySourceArn :: Prelude.Text,
    -- | A list of names of API operations to evaluate in the simulation. Each
    -- operation is evaluated for each resource. Each operation must include
    -- the service identifier, such as @iam:CreateUser@.
    actionNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulatePrincipalPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callerArn', 'simulatePrincipalPolicy_callerArn' - The ARN of the IAM user that you want to specify as the simulated caller
-- of the API operations. If you do not specify a @CallerArn@, it defaults
-- to the ARN of the user that you specify in @PolicySourceArn@, if you
-- specified a user. If you include both a @PolicySourceArn@ (for example,
-- @arn:aws:iam::123456789012:user\/David@) and a @CallerArn@ (for example,
-- @arn:aws:iam::123456789012:user\/Bob@), the result is that you simulate
-- calling the API operations as Bob, as if Bob had David\'s policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN
-- of an assumed role, federated user, or a service principal.
--
-- @CallerArn@ is required if you include a @ResourcePolicy@ and the
-- @PolicySourceArn@ is not the ARN for an IAM user. This is required so
-- that the resource-based policy\'s @Principal@ element has a value to use
-- in evaluating the policy.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'contextEntries', 'simulatePrincipalPolicy_contextEntries' - A list of context keys and corresponding values for the simulation to
-- use. Whenever a context key is evaluated in one of the simulated IAM
-- permissions policies, the corresponding value is supplied.
--
-- 'marker', 'simulatePrincipalPolicy_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'simulatePrincipalPolicy_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'permissionsBoundaryPolicyInputList', 'simulatePrincipalPolicy_permissionsBoundaryPolicyInputList' - The IAM permissions boundary policy to simulate. The permissions
-- boundary sets the maximum permissions that the entity can have. You can
-- input only one permissions boundary when you pass a policy to this
-- operation. An IAM entity can only have one permissions boundary in
-- effect at a time. For example, if a permissions boundary is attached to
-- an entity and you pass in a different permissions boundary policy using
-- this parameter, then the new permissions boundary policy is used for the
-- simulation. For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
-- in the /IAM User Guide/. The policy input is specified as a string
-- containing the complete, valid JSON text of a permissions boundary
-- policy.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- 'policyInputList', 'simulatePrincipalPolicy_policyInputList' - An optional list of additional policy documents to include in the
-- simulation. Each document is specified as a string containing the
-- complete, valid JSON text of an IAM policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- 'resourceArns', 'simulatePrincipalPolicy_resourceArns' - A list of ARNs of Amazon Web Services resources to include in the
-- simulation. If this parameter is not provided, then the value defaults
-- to @*@ (all resources). Each API in the @ActionNames@ parameter is
-- evaluated for each resource in this list. The simulation determines the
-- access result (allowed or denied) of each combination and reports it in
-- the response. You can simulate resources that don\'t exist in your
-- account.
--
-- The simulation does not automatically retrieve policies for the
-- specified resources. If you want to include a resource policy in the
-- simulation, then you must include the policy as a string in the
-- @ResourcePolicy@ parameter.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'resourceHandlingOption', 'simulatePrincipalPolicy_resourceHandlingOption' - Specifies the type of simulation to run. Different API operations that
-- support resource-based policies require different combinations of
-- resources. By specifying the type of simulation to run, you enable the
-- policy simulator to enforce the presence of the required resources to
-- ensure reliable simulation results. If your simulation does not match
-- one of the following scenarios, then you can omit this parameter. The
-- following list shows each of the supported scenario values and the
-- resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and
-- security group resources. If your scenario includes an EBS volume, then
-- you must specify that volume as a resource. If the EC2 scenario includes
-- VPC, then you must supply the network interface resource. If it includes
-- an IP subnet, then you must specify the subnet resource. For more
-- information on the EC2 scenario options, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported platforms>
-- in the /Amazon EC2 User Guide/.
--
-- -   __EC2-VPC-InstanceStore__
--
--     instance, image, security group, network interface
--
-- -   __EC2-VPC-InstanceStore-Subnet__
--
--     instance, image, security group, network interface, subnet
--
-- -   __EC2-VPC-EBS__
--
--     instance, image, security group, network interface, volume
--
-- -   __EC2-VPC-EBS-Subnet__
--
--     instance, image, security group, network interface, subnet, volume
--
-- 'resourceOwner', 'simulatePrincipalPolicy_resourceOwner' - An Amazon Web Services account ID that specifies the owner of any
-- simulated resource that does not identify its owner in the resource ARN.
-- Examples of resource ARNs include an S3 bucket or object. If
-- @ResourceOwner@ is specified, it is also used as the account owner of
-- any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@
-- parameter is not specified, then the owner of the resources and the
-- resource policy defaults to the account of the identity provided in
-- @CallerArn@. This parameter is required only if you specify a
-- resource-based policy and account that owns the resource is different
-- from the account that owns the simulated calling user @CallerArn@.
--
-- 'resourcePolicy', 'simulatePrincipalPolicy_resourcePolicy' - A resource-based policy to include in the simulation provided as a
-- string. Each resource in the simulation is treated as if it had this
-- policy attached. You can include only one resource-based policy in a
-- simulation.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- 'policySourceArn', 'simulatePrincipalPolicy_policySourceArn' - The Amazon Resource Name (ARN) of a user, group, or role whose policies
-- you want to include in the simulation. If you specify a user, group, or
-- role, the simulation includes all policies that are associated with that
-- entity. If you specify a user, the simulation also includes all policies
-- that are attached to any groups the user belongs to.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'actionNames', 'simulatePrincipalPolicy_actionNames' - A list of names of API operations to evaluate in the simulation. Each
-- operation is evaluated for each resource. Each operation must include
-- the service identifier, such as @iam:CreateUser@.
newSimulatePrincipalPolicy ::
  -- | 'policySourceArn'
  Prelude.Text ->
  SimulatePrincipalPolicy
newSimulatePrincipalPolicy pPolicySourceArn_ =
  SimulatePrincipalPolicy'
    { callerArn =
        Prelude.Nothing,
      contextEntries = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      permissionsBoundaryPolicyInputList =
        Prelude.Nothing,
      policyInputList = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      resourceHandlingOption = Prelude.Nothing,
      resourceOwner = Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      policySourceArn = pPolicySourceArn_,
      actionNames = Prelude.mempty
    }

-- | The ARN of the IAM user that you want to specify as the simulated caller
-- of the API operations. If you do not specify a @CallerArn@, it defaults
-- to the ARN of the user that you specify in @PolicySourceArn@, if you
-- specified a user. If you include both a @PolicySourceArn@ (for example,
-- @arn:aws:iam::123456789012:user\/David@) and a @CallerArn@ (for example,
-- @arn:aws:iam::123456789012:user\/Bob@), the result is that you simulate
-- calling the API operations as Bob, as if Bob had David\'s policies.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN
-- of an assumed role, federated user, or a service principal.
--
-- @CallerArn@ is required if you include a @ResourcePolicy@ and the
-- @PolicySourceArn@ is not the ARN for an IAM user. This is required so
-- that the resource-based policy\'s @Principal@ element has a value to use
-- in evaluating the policy.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
simulatePrincipalPolicy_callerArn :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe Prelude.Text)
simulatePrincipalPolicy_callerArn = Lens.lens (\SimulatePrincipalPolicy' {callerArn} -> callerArn) (\s@SimulatePrincipalPolicy' {} a -> s {callerArn = a} :: SimulatePrincipalPolicy)

-- | A list of context keys and corresponding values for the simulation to
-- use. Whenever a context key is evaluated in one of the simulated IAM
-- permissions policies, the corresponding value is supplied.
simulatePrincipalPolicy_contextEntries :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe [ContextEntry])
simulatePrincipalPolicy_contextEntries = Lens.lens (\SimulatePrincipalPolicy' {contextEntries} -> contextEntries) (\s@SimulatePrincipalPolicy' {} a -> s {contextEntries = a} :: SimulatePrincipalPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
simulatePrincipalPolicy_marker :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe Prelude.Text)
simulatePrincipalPolicy_marker = Lens.lens (\SimulatePrincipalPolicy' {marker} -> marker) (\s@SimulatePrincipalPolicy' {} a -> s {marker = a} :: SimulatePrincipalPolicy)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
simulatePrincipalPolicy_maxItems :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe Prelude.Natural)
simulatePrincipalPolicy_maxItems = Lens.lens (\SimulatePrincipalPolicy' {maxItems} -> maxItems) (\s@SimulatePrincipalPolicy' {} a -> s {maxItems = a} :: SimulatePrincipalPolicy)

-- | The IAM permissions boundary policy to simulate. The permissions
-- boundary sets the maximum permissions that the entity can have. You can
-- input only one permissions boundary when you pass a policy to this
-- operation. An IAM entity can only have one permissions boundary in
-- effect at a time. For example, if a permissions boundary is attached to
-- an entity and you pass in a different permissions boundary policy using
-- this parameter, then the new permissions boundary policy is used for the
-- simulation. For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
-- in the /IAM User Guide/. The policy input is specified as a string
-- containing the complete, valid JSON text of a permissions boundary
-- policy.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
simulatePrincipalPolicy_permissionsBoundaryPolicyInputList :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe [Prelude.Text])
simulatePrincipalPolicy_permissionsBoundaryPolicyInputList = Lens.lens (\SimulatePrincipalPolicy' {permissionsBoundaryPolicyInputList} -> permissionsBoundaryPolicyInputList) (\s@SimulatePrincipalPolicy' {} a -> s {permissionsBoundaryPolicyInputList = a} :: SimulatePrincipalPolicy) Prelude.. Lens.mapping Lens.coerced

-- | An optional list of additional policy documents to include in the
-- simulation. Each document is specified as a string containing the
-- complete, valid JSON text of an IAM policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
simulatePrincipalPolicy_policyInputList :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe [Prelude.Text])
simulatePrincipalPolicy_policyInputList = Lens.lens (\SimulatePrincipalPolicy' {policyInputList} -> policyInputList) (\s@SimulatePrincipalPolicy' {} a -> s {policyInputList = a} :: SimulatePrincipalPolicy) Prelude.. Lens.mapping Lens.coerced

-- | A list of ARNs of Amazon Web Services resources to include in the
-- simulation. If this parameter is not provided, then the value defaults
-- to @*@ (all resources). Each API in the @ActionNames@ parameter is
-- evaluated for each resource in this list. The simulation determines the
-- access result (allowed or denied) of each combination and reports it in
-- the response. You can simulate resources that don\'t exist in your
-- account.
--
-- The simulation does not automatically retrieve policies for the
-- specified resources. If you want to include a resource policy in the
-- simulation, then you must include the policy as a string in the
-- @ResourcePolicy@ parameter.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
simulatePrincipalPolicy_resourceArns :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe [Prelude.Text])
simulatePrincipalPolicy_resourceArns = Lens.lens (\SimulatePrincipalPolicy' {resourceArns} -> resourceArns) (\s@SimulatePrincipalPolicy' {} a -> s {resourceArns = a} :: SimulatePrincipalPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of simulation to run. Different API operations that
-- support resource-based policies require different combinations of
-- resources. By specifying the type of simulation to run, you enable the
-- policy simulator to enforce the presence of the required resources to
-- ensure reliable simulation results. If your simulation does not match
-- one of the following scenarios, then you can omit this parameter. The
-- following list shows each of the supported scenario values and the
-- resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and
-- security group resources. If your scenario includes an EBS volume, then
-- you must specify that volume as a resource. If the EC2 scenario includes
-- VPC, then you must supply the network interface resource. If it includes
-- an IP subnet, then you must specify the subnet resource. For more
-- information on the EC2 scenario options, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported platforms>
-- in the /Amazon EC2 User Guide/.
--
-- -   __EC2-VPC-InstanceStore__
--
--     instance, image, security group, network interface
--
-- -   __EC2-VPC-InstanceStore-Subnet__
--
--     instance, image, security group, network interface, subnet
--
-- -   __EC2-VPC-EBS__
--
--     instance, image, security group, network interface, volume
--
-- -   __EC2-VPC-EBS-Subnet__
--
--     instance, image, security group, network interface, subnet, volume
simulatePrincipalPolicy_resourceHandlingOption :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe Prelude.Text)
simulatePrincipalPolicy_resourceHandlingOption = Lens.lens (\SimulatePrincipalPolicy' {resourceHandlingOption} -> resourceHandlingOption) (\s@SimulatePrincipalPolicy' {} a -> s {resourceHandlingOption = a} :: SimulatePrincipalPolicy)

-- | An Amazon Web Services account ID that specifies the owner of any
-- simulated resource that does not identify its owner in the resource ARN.
-- Examples of resource ARNs include an S3 bucket or object. If
-- @ResourceOwner@ is specified, it is also used as the account owner of
-- any @ResourcePolicy@ included in the simulation. If the @ResourceOwner@
-- parameter is not specified, then the owner of the resources and the
-- resource policy defaults to the account of the identity provided in
-- @CallerArn@. This parameter is required only if you specify a
-- resource-based policy and account that owns the resource is different
-- from the account that owns the simulated calling user @CallerArn@.
simulatePrincipalPolicy_resourceOwner :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe Prelude.Text)
simulatePrincipalPolicy_resourceOwner = Lens.lens (\SimulatePrincipalPolicy' {resourceOwner} -> resourceOwner) (\s@SimulatePrincipalPolicy' {} a -> s {resourceOwner = a} :: SimulatePrincipalPolicy)

-- | A resource-based policy to include in the simulation provided as a
-- string. Each resource in the simulation is treated as if it had this
-- policy attached. You can include only one resource-based policy in a
-- simulation.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
simulatePrincipalPolicy_resourcePolicy :: Lens.Lens' SimulatePrincipalPolicy (Prelude.Maybe Prelude.Text)
simulatePrincipalPolicy_resourcePolicy = Lens.lens (\SimulatePrincipalPolicy' {resourcePolicy} -> resourcePolicy) (\s@SimulatePrincipalPolicy' {} a -> s {resourcePolicy = a} :: SimulatePrincipalPolicy)

-- | The Amazon Resource Name (ARN) of a user, group, or role whose policies
-- you want to include in the simulation. If you specify a user, group, or
-- role, the simulation includes all policies that are associated with that
-- entity. If you specify a user, the simulation also includes all policies
-- that are attached to any groups the user belongs to.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
simulatePrincipalPolicy_policySourceArn :: Lens.Lens' SimulatePrincipalPolicy Prelude.Text
simulatePrincipalPolicy_policySourceArn = Lens.lens (\SimulatePrincipalPolicy' {policySourceArn} -> policySourceArn) (\s@SimulatePrincipalPolicy' {} a -> s {policySourceArn = a} :: SimulatePrincipalPolicy)

-- | A list of names of API operations to evaluate in the simulation. Each
-- operation is evaluated for each resource. Each operation must include
-- the service identifier, such as @iam:CreateUser@.
simulatePrincipalPolicy_actionNames :: Lens.Lens' SimulatePrincipalPolicy [Prelude.Text]
simulatePrincipalPolicy_actionNames = Lens.lens (\SimulatePrincipalPolicy' {actionNames} -> actionNames) (\s@SimulatePrincipalPolicy' {} a -> s {actionNames = a} :: SimulatePrincipalPolicy) Prelude.. Lens.coerced

instance Core.AWSPager SimulatePrincipalPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? simulatePolicyResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? simulatePolicyResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& simulatePrincipalPolicy_marker
              Lens..~ rs
              Lens.^? simulatePolicyResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest SimulatePrincipalPolicy where
  type
    AWSResponse SimulatePrincipalPolicy =
      SimulatePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SimulatePrincipalPolicyResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable SimulatePrincipalPolicy where
  hashWithSalt _salt SimulatePrincipalPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` callerArn
      `Prelude.hashWithSalt` contextEntries
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` permissionsBoundaryPolicyInputList
      `Prelude.hashWithSalt` policyInputList
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` resourceHandlingOption
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` resourcePolicy
      `Prelude.hashWithSalt` policySourceArn
      `Prelude.hashWithSalt` actionNames

instance Prelude.NFData SimulatePrincipalPolicy where
  rnf SimulatePrincipalPolicy' {..} =
    Prelude.rnf callerArn `Prelude.seq`
      Prelude.rnf contextEntries `Prelude.seq`
        Prelude.rnf marker `Prelude.seq`
          Prelude.rnf maxItems `Prelude.seq`
            Prelude.rnf permissionsBoundaryPolicyInputList `Prelude.seq`
              Prelude.rnf policyInputList `Prelude.seq`
                Prelude.rnf resourceArns `Prelude.seq`
                  Prelude.rnf resourceHandlingOption `Prelude.seq`
                    Prelude.rnf resourceOwner `Prelude.seq`
                      Prelude.rnf resourcePolicy `Prelude.seq`
                        Prelude.rnf policySourceArn `Prelude.seq`
                          Prelude.rnf actionNames

instance Data.ToHeaders SimulatePrincipalPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SimulatePrincipalPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery SimulatePrincipalPolicy where
  toQuery SimulatePrincipalPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SimulatePrincipalPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "CallerArn" Data.=: callerArn,
        "ContextEntries"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> contextEntries
            ),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PermissionsBoundaryPolicyInputList"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> permissionsBoundaryPolicyInputList
            ),
        "PolicyInputList"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> policyInputList
            ),
        "ResourceArns"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> resourceArns),
        "ResourceHandlingOption"
          Data.=: resourceHandlingOption,
        "ResourceOwner" Data.=: resourceOwner,
        "ResourcePolicy" Data.=: resourcePolicy,
        "PolicySourceArn" Data.=: policySourceArn,
        "ActionNames"
          Data.=: Data.toQueryList "member" actionNames
      ]
