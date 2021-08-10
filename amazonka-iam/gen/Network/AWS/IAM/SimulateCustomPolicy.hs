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
-- Module      : Network.AWS.IAM.SimulateCustomPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate how a set of IAM policies and optionally a resource-based
-- policy works with a list of API operations and AWS resources to
-- determine the policies\' effective permissions. The policies are
-- provided as strings.
--
-- The simulation does not perform the API operations; it only checks the
-- authorization to determine if the simulated policies allow or deny the
-- operations. You can simulate resources that don\'t exist in your
-- account.
--
-- If you want to simulate existing policies that are attached to an IAM
-- user, group, or role, use SimulatePrincipalPolicy instead.
--
-- Context keys are variables that are maintained by AWS and its services
-- and which provide details about the context of an API query request. You
-- can use the @Condition@ element of an IAM policy to evaluate context
-- keys. To get the list of context keys that the policies require for
-- correct simulation, use GetContextKeysForCustomPolicy.
--
-- If the output is long, you can use @MaxItems@ and @Marker@ parameters to
-- paginate the results.
--
-- For more information about using the policy simulator, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_testing-policies.html Testing IAM policies with the IAM policy simulator>
-- in the /IAM User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.IAM.SimulateCustomPolicy
  ( -- * Creating a Request
    SimulateCustomPolicy (..),
    newSimulateCustomPolicy,

    -- * Request Lenses
    simulateCustomPolicy_resourceOwner,
    simulateCustomPolicy_contextEntries,
    simulateCustomPolicy_resourcePolicy,
    simulateCustomPolicy_resourceArns,
    simulateCustomPolicy_permissionsBoundaryPolicyInputList,
    simulateCustomPolicy_resourceHandlingOption,
    simulateCustomPolicy_callerArn,
    simulateCustomPolicy_maxItems,
    simulateCustomPolicy_marker,
    simulateCustomPolicy_policyInputList,
    simulateCustomPolicy_actionNames,

    -- * Destructuring the Response
    SimulatePolicyResponse (..),
    newSimulatePolicyResponse,

    -- * Response Lenses
    simulatePolicyResponse_isTruncated,
    simulatePolicyResponse_evaluationResults,
    simulatePolicyResponse_marker,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSimulateCustomPolicy' smart constructor.
data SimulateCustomPolicy = SimulateCustomPolicy'
  { -- | An ARN representing the AWS account ID that specifies the owner of any
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
    -- The ARN for an account uses the following syntax:
    -- @arn:aws:iam::AWS-account-ID:root@. For example, to represent the
    -- account with the 112233445566 ID, use the following ARN:
    -- @arn:aws:iam::112233445566-ID:root@.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | A list of context keys and corresponding values for the simulation to
    -- use. Whenever a context key is evaluated in one of the simulated IAM
    -- permissions policies, the corresponding value is supplied.
    contextEntries :: Prelude.Maybe [ContextEntry],
    -- | A resource-based policy to include in the simulation provided as a
    -- string. Each resource in the simulation is treated as if it had this
    -- policy attached. You can include only one resource-based policy in a
    -- simulation.
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
    -- | A list of ARNs of AWS resources to include in the simulation. If this
    -- parameter is not provided, then the value defaults to @*@ (all
    -- resources). Each API in the @ActionNames@ parameter is evaluated for
    -- each resource in this list. The simulation determines the access result
    -- (allowed or denied) of each combination and reports it in the response.
    -- You can simulate resources that don\'t exist in your account.
    --
    -- The simulation does not automatically retrieve policies for the
    -- specified resources. If you want to include a resource policy in the
    -- simulation, then you must include the policy as a string in the
    -- @ResourcePolicy@ parameter.
    --
    -- If you include a @ResourcePolicy@, then it must be applicable to all of
    -- the resources included in the simulation or you receive an invalid input
    -- error.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The IAM permissions boundary policy to simulate. The permissions
    -- boundary sets the maximum permissions that an IAM entity can have. You
    -- can input only one permissions boundary when you pass a policy to this
    -- operation. For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
    -- in the /IAM User Guide/. The policy input is specified as a string that
    -- contains the complete, valid JSON text of a permissions boundary policy.
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
    -- security-group resources. If your scenario includes an EBS volume, then
    -- you must specify that volume as a resource. If the EC2 scenario includes
    -- VPC, then you must supply the network-interface resource. If it includes
    -- an IP subnet, then you must specify the subnet resource. For more
    -- information on the EC2 scenario options, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported platforms>
    -- in the /Amazon EC2 User Guide/.
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
    resourceHandlingOption :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM user that you want to use as the simulated caller of
    -- the API operations. @CallerArn@ is required if you include a
    -- @ResourcePolicy@ so that the policy\'s @Principal@ element has a value
    -- to use in evaluating the policy.
    --
    -- You can specify only the ARN of an IAM user. You cannot specify the ARN
    -- of an assumed role, federated user, or a service principal.
    callerArn :: Prelude.Maybe Prelude.Text,
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
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of policy documents to include in the simulation. Each document
    -- is specified as a string containing the complete, valid JSON text of an
    -- IAM policy. Do not include any resource-based policies in this
    -- parameter. Any resource-based policy must be submitted with the
    -- @ResourcePolicy@ parameter. The policies cannot be \"scope-down\"
    -- policies, such as you could include in a call to
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken>
    -- or one of the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole>
    -- API operations. In other words, do not use policies designed to restrict
    -- what a user can do while using the temporary credentials.
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
    policyInputList :: [Prelude.Text],
    -- | A list of names of API operations to evaluate in the simulation. Each
    -- operation is evaluated against each resource. Each operation must
    -- include the service identifier, such as @iam:CreateUser@. This operation
    -- does not support using wildcards (*) in an action name.
    actionNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulateCustomPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceOwner', 'simulateCustomPolicy_resourceOwner' - An ARN representing the AWS account ID that specifies the owner of any
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
-- The ARN for an account uses the following syntax:
-- @arn:aws:iam::AWS-account-ID:root@. For example, to represent the
-- account with the 112233445566 ID, use the following ARN:
-- @arn:aws:iam::112233445566-ID:root@.
--
-- 'contextEntries', 'simulateCustomPolicy_contextEntries' - A list of context keys and corresponding values for the simulation to
-- use. Whenever a context key is evaluated in one of the simulated IAM
-- permissions policies, the corresponding value is supplied.
--
-- 'resourcePolicy', 'simulateCustomPolicy_resourcePolicy' - A resource-based policy to include in the simulation provided as a
-- string. Each resource in the simulation is treated as if it had this
-- policy attached. You can include only one resource-based policy in a
-- simulation.
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
-- 'resourceArns', 'simulateCustomPolicy_resourceArns' - A list of ARNs of AWS resources to include in the simulation. If this
-- parameter is not provided, then the value defaults to @*@ (all
-- resources). Each API in the @ActionNames@ parameter is evaluated for
-- each resource in this list. The simulation determines the access result
-- (allowed or denied) of each combination and reports it in the response.
-- You can simulate resources that don\'t exist in your account.
--
-- The simulation does not automatically retrieve policies for the
-- specified resources. If you want to include a resource policy in the
-- simulation, then you must include the policy as a string in the
-- @ResourcePolicy@ parameter.
--
-- If you include a @ResourcePolicy@, then it must be applicable to all of
-- the resources included in the simulation or you receive an invalid input
-- error.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'permissionsBoundaryPolicyInputList', 'simulateCustomPolicy_permissionsBoundaryPolicyInputList' - The IAM permissions boundary policy to simulate. The permissions
-- boundary sets the maximum permissions that an IAM entity can have. You
-- can input only one permissions boundary when you pass a policy to this
-- operation. For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
-- in the /IAM User Guide/. The policy input is specified as a string that
-- contains the complete, valid JSON text of a permissions boundary policy.
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
-- 'resourceHandlingOption', 'simulateCustomPolicy_resourceHandlingOption' - Specifies the type of simulation to run. Different API operations that
-- support resource-based policies require different combinations of
-- resources. By specifying the type of simulation to run, you enable the
-- policy simulator to enforce the presence of the required resources to
-- ensure reliable simulation results. If your simulation does not match
-- one of the following scenarios, then you can omit this parameter. The
-- following list shows each of the supported scenario values and the
-- resources that you must define to run the simulation.
--
-- Each of the EC2 scenarios requires that you specify instance, image, and
-- security-group resources. If your scenario includes an EBS volume, then
-- you must specify that volume as a resource. If the EC2 scenario includes
-- VPC, then you must supply the network-interface resource. If it includes
-- an IP subnet, then you must specify the subnet resource. For more
-- information on the EC2 scenario options, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported platforms>
-- in the /Amazon EC2 User Guide/.
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
-- 'callerArn', 'simulateCustomPolicy_callerArn' - The ARN of the IAM user that you want to use as the simulated caller of
-- the API operations. @CallerArn@ is required if you include a
-- @ResourcePolicy@ so that the policy\'s @Principal@ element has a value
-- to use in evaluating the policy.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN
-- of an assumed role, federated user, or a service principal.
--
-- 'maxItems', 'simulateCustomPolicy_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'simulateCustomPolicy_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'policyInputList', 'simulateCustomPolicy_policyInputList' - A list of policy documents to include in the simulation. Each document
-- is specified as a string containing the complete, valid JSON text of an
-- IAM policy. Do not include any resource-based policies in this
-- parameter. Any resource-based policy must be submitted with the
-- @ResourcePolicy@ parameter. The policies cannot be \"scope-down\"
-- policies, such as you could include in a call to
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken>
-- or one of the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole>
-- API operations. In other words, do not use policies designed to restrict
-- what a user can do while using the temporary credentials.
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
-- 'actionNames', 'simulateCustomPolicy_actionNames' - A list of names of API operations to evaluate in the simulation. Each
-- operation is evaluated against each resource. Each operation must
-- include the service identifier, such as @iam:CreateUser@. This operation
-- does not support using wildcards (*) in an action name.
newSimulateCustomPolicy ::
  SimulateCustomPolicy
newSimulateCustomPolicy =
  SimulateCustomPolicy'
    { resourceOwner =
        Prelude.Nothing,
      contextEntries = Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      permissionsBoundaryPolicyInputList = Prelude.Nothing,
      resourceHandlingOption = Prelude.Nothing,
      callerArn = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      policyInputList = Prelude.mempty,
      actionNames = Prelude.mempty
    }

-- | An ARN representing the AWS account ID that specifies the owner of any
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
-- The ARN for an account uses the following syntax:
-- @arn:aws:iam::AWS-account-ID:root@. For example, to represent the
-- account with the 112233445566 ID, use the following ARN:
-- @arn:aws:iam::112233445566-ID:root@.
simulateCustomPolicy_resourceOwner :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe Prelude.Text)
simulateCustomPolicy_resourceOwner = Lens.lens (\SimulateCustomPolicy' {resourceOwner} -> resourceOwner) (\s@SimulateCustomPolicy' {} a -> s {resourceOwner = a} :: SimulateCustomPolicy)

-- | A list of context keys and corresponding values for the simulation to
-- use. Whenever a context key is evaluated in one of the simulated IAM
-- permissions policies, the corresponding value is supplied.
simulateCustomPolicy_contextEntries :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe [ContextEntry])
simulateCustomPolicy_contextEntries = Lens.lens (\SimulateCustomPolicy' {contextEntries} -> contextEntries) (\s@SimulateCustomPolicy' {} a -> s {contextEntries = a} :: SimulateCustomPolicy) Prelude.. Lens.mapping Lens._Coerce

-- | A resource-based policy to include in the simulation provided as a
-- string. Each resource in the simulation is treated as if it had this
-- policy attached. You can include only one resource-based policy in a
-- simulation.
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
simulateCustomPolicy_resourcePolicy :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe Prelude.Text)
simulateCustomPolicy_resourcePolicy = Lens.lens (\SimulateCustomPolicy' {resourcePolicy} -> resourcePolicy) (\s@SimulateCustomPolicy' {} a -> s {resourcePolicy = a} :: SimulateCustomPolicy)

-- | A list of ARNs of AWS resources to include in the simulation. If this
-- parameter is not provided, then the value defaults to @*@ (all
-- resources). Each API in the @ActionNames@ parameter is evaluated for
-- each resource in this list. The simulation determines the access result
-- (allowed or denied) of each combination and reports it in the response.
-- You can simulate resources that don\'t exist in your account.
--
-- The simulation does not automatically retrieve policies for the
-- specified resources. If you want to include a resource policy in the
-- simulation, then you must include the policy as a string in the
-- @ResourcePolicy@ parameter.
--
-- If you include a @ResourcePolicy@, then it must be applicable to all of
-- the resources included in the simulation or you receive an invalid input
-- error.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
simulateCustomPolicy_resourceArns :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe [Prelude.Text])
simulateCustomPolicy_resourceArns = Lens.lens (\SimulateCustomPolicy' {resourceArns} -> resourceArns) (\s@SimulateCustomPolicy' {} a -> s {resourceArns = a} :: SimulateCustomPolicy) Prelude.. Lens.mapping Lens._Coerce

-- | The IAM permissions boundary policy to simulate. The permissions
-- boundary sets the maximum permissions that an IAM entity can have. You
-- can input only one permissions boundary when you pass a policy to this
-- operation. For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM entities>
-- in the /IAM User Guide/. The policy input is specified as a string that
-- contains the complete, valid JSON text of a permissions boundary policy.
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
simulateCustomPolicy_permissionsBoundaryPolicyInputList :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe [Prelude.Text])
simulateCustomPolicy_permissionsBoundaryPolicyInputList = Lens.lens (\SimulateCustomPolicy' {permissionsBoundaryPolicyInputList} -> permissionsBoundaryPolicyInputList) (\s@SimulateCustomPolicy' {} a -> s {permissionsBoundaryPolicyInputList = a} :: SimulateCustomPolicy) Prelude.. Lens.mapping Lens._Coerce

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
-- security-group resources. If your scenario includes an EBS volume, then
-- you must specify that volume as a resource. If the EC2 scenario includes
-- VPC, then you must supply the network-interface resource. If it includes
-- an IP subnet, then you must specify the subnet resource. For more
-- information on the EC2 scenario options, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-supported-platforms.html Supported platforms>
-- in the /Amazon EC2 User Guide/.
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
simulateCustomPolicy_resourceHandlingOption :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe Prelude.Text)
simulateCustomPolicy_resourceHandlingOption = Lens.lens (\SimulateCustomPolicy' {resourceHandlingOption} -> resourceHandlingOption) (\s@SimulateCustomPolicy' {} a -> s {resourceHandlingOption = a} :: SimulateCustomPolicy)

-- | The ARN of the IAM user that you want to use as the simulated caller of
-- the API operations. @CallerArn@ is required if you include a
-- @ResourcePolicy@ so that the policy\'s @Principal@ element has a value
-- to use in evaluating the policy.
--
-- You can specify only the ARN of an IAM user. You cannot specify the ARN
-- of an assumed role, federated user, or a service principal.
simulateCustomPolicy_callerArn :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe Prelude.Text)
simulateCustomPolicy_callerArn = Lens.lens (\SimulateCustomPolicy' {callerArn} -> callerArn) (\s@SimulateCustomPolicy' {} a -> s {callerArn = a} :: SimulateCustomPolicy)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
simulateCustomPolicy_maxItems :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe Prelude.Natural)
simulateCustomPolicy_maxItems = Lens.lens (\SimulateCustomPolicy' {maxItems} -> maxItems) (\s@SimulateCustomPolicy' {} a -> s {maxItems = a} :: SimulateCustomPolicy)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
simulateCustomPolicy_marker :: Lens.Lens' SimulateCustomPolicy (Prelude.Maybe Prelude.Text)
simulateCustomPolicy_marker = Lens.lens (\SimulateCustomPolicy' {marker} -> marker) (\s@SimulateCustomPolicy' {} a -> s {marker = a} :: SimulateCustomPolicy)

-- | A list of policy documents to include in the simulation. Each document
-- is specified as a string containing the complete, valid JSON text of an
-- IAM policy. Do not include any resource-based policies in this
-- parameter. Any resource-based policy must be submitted with the
-- @ResourcePolicy@ parameter. The policies cannot be \"scope-down\"
-- policies, such as you could include in a call to
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_GetFederationToken.html GetFederationToken>
-- or one of the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_AssumeRole.html AssumeRole>
-- API operations. In other words, do not use policies designed to restrict
-- what a user can do while using the temporary credentials.
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
simulateCustomPolicy_policyInputList :: Lens.Lens' SimulateCustomPolicy [Prelude.Text]
simulateCustomPolicy_policyInputList = Lens.lens (\SimulateCustomPolicy' {policyInputList} -> policyInputList) (\s@SimulateCustomPolicy' {} a -> s {policyInputList = a} :: SimulateCustomPolicy) Prelude.. Lens._Coerce

-- | A list of names of API operations to evaluate in the simulation. Each
-- operation is evaluated against each resource. Each operation must
-- include the service identifier, such as @iam:CreateUser@. This operation
-- does not support using wildcards (*) in an action name.
simulateCustomPolicy_actionNames :: Lens.Lens' SimulateCustomPolicy [Prelude.Text]
simulateCustomPolicy_actionNames = Lens.lens (\SimulateCustomPolicy' {actionNames} -> actionNames) (\s@SimulateCustomPolicy' {} a -> s {actionNames = a} :: SimulateCustomPolicy) Prelude.. Lens._Coerce

instance Core.AWSPager SimulateCustomPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? simulatePolicyResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? simulatePolicyResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& simulateCustomPolicy_marker
          Lens..~ rs
          Lens.^? simulatePolicyResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest SimulateCustomPolicy where
  type
    AWSResponse SimulateCustomPolicy =
      SimulatePolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SimulateCustomPolicyResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable SimulateCustomPolicy

instance Prelude.NFData SimulateCustomPolicy

instance Core.ToHeaders SimulateCustomPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SimulateCustomPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery SimulateCustomPolicy where
  toQuery SimulateCustomPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("SimulateCustomPolicy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "ResourceOwner" Core.=: resourceOwner,
        "ContextEntries"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> contextEntries
            ),
        "ResourcePolicy" Core.=: resourcePolicy,
        "ResourceArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> resourceArns),
        "PermissionsBoundaryPolicyInputList"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> permissionsBoundaryPolicyInputList
            ),
        "ResourceHandlingOption"
          Core.=: resourceHandlingOption,
        "CallerArn" Core.=: callerArn,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "PolicyInputList"
          Core.=: Core.toQueryList "member" policyInputList,
        "ActionNames"
          Core.=: Core.toQueryList "member" actionNames
      ]
