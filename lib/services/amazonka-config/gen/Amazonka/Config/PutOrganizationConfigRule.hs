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
-- Module      : Amazonka.Config.PutOrganizationConfigRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an Config rule for your entire organization to evaluate
-- if your Amazon Web Services resources comply with your desired
-- configurations. For information on how many organization Config rules
-- you can have per account, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/configlimits.html Service Limits>
-- in the /Config Developer Guide/.
--
-- Only a management account and a delegated administrator can create or
-- update an organization Config rule. When calling this API with a
-- delegated administrator, you must ensure Organizations
-- @ListDelegatedAdministrator@ permissions are added. An organization can
-- have up to 3 delegated administrators.
--
-- This API enables organization service access through the
-- @EnableAWSServiceAccess@ action and creates a service-linked role
-- @AWSServiceRoleForConfigMultiAccountSetup@ in the management or
-- delegated administrator account of your organization. The service-linked
-- role is created only when the role does not exist in the caller account.
-- Config verifies the existence of role with @GetRole@ action.
--
-- To use this API with delegated administrator, register a delegated
-- administrator by calling Amazon Web Services Organization
-- @register-delegated-administrator@ for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- There are two types of rules: /Config Managed Rules/ and /Config Custom
-- Rules/. You can use @PutOrganizationConfigRule@ to create both Config
-- Managed Rules and Config Custom Rules.
--
-- Config Managed Rules are predefined, customizable rules created by
-- Config. For a list of managed rules, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html List of Config Managed Rules>.
-- If you are adding an Config managed rule, you must specify the rule\'s
-- identifier for the @RuleIdentifier@ key.
--
-- Config Custom Rules are rules that you create from scratch. There are
-- two ways to create Config custom rules: with Lambda functions (
-- <https://docs.aws.amazon.com/config/latest/developerguide/gettingstarted-concepts.html#gettingstarted-concepts-function Lambda Developer Guide>)
-- and with Guard
-- (<https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>),
-- a policy-as-code language. Config custom rules created with Lambda are
-- called /Config Custom Lambda Rules/ and Config custom rules created with
-- Guard are called /Config Custom Policy Rules/.
--
-- If you are adding a new Config Custom Lambda rule, you first need to
-- create an Lambda function in the management account or a delegated
-- administrator that the rule invokes to evaluate your resources. You also
-- need to create an IAM role in the managed account that can be assumed by
-- the Lambda function. When you use @PutOrganizationConfigRule@ to add a
-- Custom Lambda rule to Config, you must specify the Amazon Resource Name
-- (ARN) that Lambda assigns to the function.
--
-- Prerequisite: Ensure you call @EnableAllFeatures@ API to enable all
-- features in an organization.
--
-- Make sure to specify one of either
-- @OrganizationCustomPolicyRuleMetadata@ for Custom Policy rules,
-- @OrganizationCustomRuleMetadata@ for Custom Lambda rules, or
-- @OrganizationManagedRuleMetadata@ for managed rules.
module Amazonka.Config.PutOrganizationConfigRule
  ( -- * Creating a Request
    PutOrganizationConfigRule (..),
    newPutOrganizationConfigRule,

    -- * Request Lenses
    putOrganizationConfigRule_excludedAccounts,
    putOrganizationConfigRule_organizationCustomPolicyRuleMetadata,
    putOrganizationConfigRule_organizationCustomRuleMetadata,
    putOrganizationConfigRule_organizationManagedRuleMetadata,
    putOrganizationConfigRule_organizationConfigRuleName,

    -- * Destructuring the Response
    PutOrganizationConfigRuleResponse (..),
    newPutOrganizationConfigRuleResponse,

    -- * Response Lenses
    putOrganizationConfigRuleResponse_organizationConfigRuleArn,
    putOrganizationConfigRuleResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutOrganizationConfigRule' smart constructor.
data PutOrganizationConfigRule = PutOrganizationConfigRule'
  { -- | A comma-separated list of accounts that you want to exclude from an
    -- organization Config rule.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | An @OrganizationCustomPolicyRuleMetadata@ object. This object specifies
    -- metadata for your organization\'s Config Custom Policy rule. The
    -- metadata includes the runtime system in use, which accounts have debug
    -- logging enabled, and other custom rule metadata, such as resource type,
    -- resource ID of Amazon Web Services resource, and organization trigger
    -- types that initiate Config to evaluate Amazon Web Services resources
    -- against a rule.
    organizationCustomPolicyRuleMetadata :: Prelude.Maybe OrganizationCustomPolicyRuleMetadata,
    -- | An @OrganizationCustomRuleMetadata@ object. This object specifies
    -- organization custom rule metadata such as resource type, resource ID of
    -- Amazon Web Services resource, Lambda function ARN, and organization
    -- trigger types that trigger Config to evaluate your Amazon Web Services
    -- resources against a rule. It also provides the frequency with which you
    -- want Config to run evaluations for the rule if the trigger type is
    -- periodic.
    organizationCustomRuleMetadata :: Prelude.Maybe OrganizationCustomRuleMetadata,
    -- | An @OrganizationManagedRuleMetadata@ object. This object specifies
    -- organization managed rule metadata such as resource type and ID of
    -- Amazon Web Services resource along with the rule identifier. It also
    -- provides the frequency with which you want Config to run evaluations for
    -- the rule if the trigger type is periodic.
    organizationManagedRuleMetadata :: Prelude.Maybe OrganizationManagedRuleMetadata,
    -- | The name that you assign to an organization Config rule.
    organizationConfigRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOrganizationConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedAccounts', 'putOrganizationConfigRule_excludedAccounts' - A comma-separated list of accounts that you want to exclude from an
-- organization Config rule.
--
-- 'organizationCustomPolicyRuleMetadata', 'putOrganizationConfigRule_organizationCustomPolicyRuleMetadata' - An @OrganizationCustomPolicyRuleMetadata@ object. This object specifies
-- metadata for your organization\'s Config Custom Policy rule. The
-- metadata includes the runtime system in use, which accounts have debug
-- logging enabled, and other custom rule metadata, such as resource type,
-- resource ID of Amazon Web Services resource, and organization trigger
-- types that initiate Config to evaluate Amazon Web Services resources
-- against a rule.
--
-- 'organizationCustomRuleMetadata', 'putOrganizationConfigRule_organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object. This object specifies
-- organization custom rule metadata such as resource type, resource ID of
-- Amazon Web Services resource, Lambda function ARN, and organization
-- trigger types that trigger Config to evaluate your Amazon Web Services
-- resources against a rule. It also provides the frequency with which you
-- want Config to run evaluations for the rule if the trigger type is
-- periodic.
--
-- 'organizationManagedRuleMetadata', 'putOrganizationConfigRule_organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object. This object specifies
-- organization managed rule metadata such as resource type and ID of
-- Amazon Web Services resource along with the rule identifier. It also
-- provides the frequency with which you want Config to run evaluations for
-- the rule if the trigger type is periodic.
--
-- 'organizationConfigRuleName', 'putOrganizationConfigRule_organizationConfigRuleName' - The name that you assign to an organization Config rule.
newPutOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  PutOrganizationConfigRule
newPutOrganizationConfigRule
  pOrganizationConfigRuleName_ =
    PutOrganizationConfigRule'
      { excludedAccounts =
          Prelude.Nothing,
        organizationCustomPolicyRuleMetadata =
          Prelude.Nothing,
        organizationCustomRuleMetadata = Prelude.Nothing,
        organizationManagedRuleMetadata =
          Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | A comma-separated list of accounts that you want to exclude from an
-- organization Config rule.
putOrganizationConfigRule_excludedAccounts :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe [Prelude.Text])
putOrganizationConfigRule_excludedAccounts = Lens.lens (\PutOrganizationConfigRule' {excludedAccounts} -> excludedAccounts) (\s@PutOrganizationConfigRule' {} a -> s {excludedAccounts = a} :: PutOrganizationConfigRule) Prelude.. Lens.mapping Lens.coerced

-- | An @OrganizationCustomPolicyRuleMetadata@ object. This object specifies
-- metadata for your organization\'s Config Custom Policy rule. The
-- metadata includes the runtime system in use, which accounts have debug
-- logging enabled, and other custom rule metadata, such as resource type,
-- resource ID of Amazon Web Services resource, and organization trigger
-- types that initiate Config to evaluate Amazon Web Services resources
-- against a rule.
putOrganizationConfigRule_organizationCustomPolicyRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe OrganizationCustomPolicyRuleMetadata)
putOrganizationConfigRule_organizationCustomPolicyRuleMetadata = Lens.lens (\PutOrganizationConfigRule' {organizationCustomPolicyRuleMetadata} -> organizationCustomPolicyRuleMetadata) (\s@PutOrganizationConfigRule' {} a -> s {organizationCustomPolicyRuleMetadata = a} :: PutOrganizationConfigRule)

-- | An @OrganizationCustomRuleMetadata@ object. This object specifies
-- organization custom rule metadata such as resource type, resource ID of
-- Amazon Web Services resource, Lambda function ARN, and organization
-- trigger types that trigger Config to evaluate your Amazon Web Services
-- resources against a rule. It also provides the frequency with which you
-- want Config to run evaluations for the rule if the trigger type is
-- periodic.
putOrganizationConfigRule_organizationCustomRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe OrganizationCustomRuleMetadata)
putOrganizationConfigRule_organizationCustomRuleMetadata = Lens.lens (\PutOrganizationConfigRule' {organizationCustomRuleMetadata} -> organizationCustomRuleMetadata) (\s@PutOrganizationConfigRule' {} a -> s {organizationCustomRuleMetadata = a} :: PutOrganizationConfigRule)

-- | An @OrganizationManagedRuleMetadata@ object. This object specifies
-- organization managed rule metadata such as resource type and ID of
-- Amazon Web Services resource along with the rule identifier. It also
-- provides the frequency with which you want Config to run evaluations for
-- the rule if the trigger type is periodic.
putOrganizationConfigRule_organizationManagedRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe OrganizationManagedRuleMetadata)
putOrganizationConfigRule_organizationManagedRuleMetadata = Lens.lens (\PutOrganizationConfigRule' {organizationManagedRuleMetadata} -> organizationManagedRuleMetadata) (\s@PutOrganizationConfigRule' {} a -> s {organizationManagedRuleMetadata = a} :: PutOrganizationConfigRule)

-- | The name that you assign to an organization Config rule.
putOrganizationConfigRule_organizationConfigRuleName :: Lens.Lens' PutOrganizationConfigRule Prelude.Text
putOrganizationConfigRule_organizationConfigRuleName = Lens.lens (\PutOrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@PutOrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: PutOrganizationConfigRule)

instance Core.AWSRequest PutOrganizationConfigRule where
  type
    AWSResponse PutOrganizationConfigRule =
      PutOrganizationConfigRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutOrganizationConfigRuleResponse'
            Prelude.<$> (x Data..?> "OrganizationConfigRuleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutOrganizationConfigRule where
  hashWithSalt _salt PutOrganizationConfigRule' {..} =
    _salt
      `Prelude.hashWithSalt` excludedAccounts
      `Prelude.hashWithSalt` organizationCustomPolicyRuleMetadata
      `Prelude.hashWithSalt` organizationCustomRuleMetadata
      `Prelude.hashWithSalt` organizationManagedRuleMetadata
      `Prelude.hashWithSalt` organizationConfigRuleName

instance Prelude.NFData PutOrganizationConfigRule where
  rnf PutOrganizationConfigRule' {..} =
    Prelude.rnf excludedAccounts
      `Prelude.seq` Prelude.rnf organizationCustomPolicyRuleMetadata
      `Prelude.seq` Prelude.rnf organizationCustomRuleMetadata
      `Prelude.seq` Prelude.rnf organizationManagedRuleMetadata
      `Prelude.seq` Prelude.rnf organizationConfigRuleName

instance Data.ToHeaders PutOrganizationConfigRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutOrganizationConfigRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutOrganizationConfigRule where
  toJSON PutOrganizationConfigRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludedAccounts" Data..=)
              Prelude.<$> excludedAccounts,
            ("OrganizationCustomPolicyRuleMetadata" Data..=)
              Prelude.<$> organizationCustomPolicyRuleMetadata,
            ("OrganizationCustomRuleMetadata" Data..=)
              Prelude.<$> organizationCustomRuleMetadata,
            ("OrganizationManagedRuleMetadata" Data..=)
              Prelude.<$> organizationManagedRuleMetadata,
            Prelude.Just
              ( "OrganizationConfigRuleName"
                  Data..= organizationConfigRuleName
              )
          ]
      )

instance Data.ToPath PutOrganizationConfigRule where
  toPath = Prelude.const "/"

instance Data.ToQuery PutOrganizationConfigRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutOrganizationConfigRuleResponse' smart constructor.
data PutOrganizationConfigRuleResponse = PutOrganizationConfigRuleResponse'
  { -- | The Amazon Resource Name (ARN) of an organization Config rule.
    organizationConfigRuleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOrganizationConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConfigRuleArn', 'putOrganizationConfigRuleResponse_organizationConfigRuleArn' - The Amazon Resource Name (ARN) of an organization Config rule.
--
-- 'httpStatus', 'putOrganizationConfigRuleResponse_httpStatus' - The response's http status code.
newPutOrganizationConfigRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutOrganizationConfigRuleResponse
newPutOrganizationConfigRuleResponse pHttpStatus_ =
  PutOrganizationConfigRuleResponse'
    { organizationConfigRuleArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of an organization Config rule.
putOrganizationConfigRuleResponse_organizationConfigRuleArn :: Lens.Lens' PutOrganizationConfigRuleResponse (Prelude.Maybe Prelude.Text)
putOrganizationConfigRuleResponse_organizationConfigRuleArn = Lens.lens (\PutOrganizationConfigRuleResponse' {organizationConfigRuleArn} -> organizationConfigRuleArn) (\s@PutOrganizationConfigRuleResponse' {} a -> s {organizationConfigRuleArn = a} :: PutOrganizationConfigRuleResponse)

-- | The response's http status code.
putOrganizationConfigRuleResponse_httpStatus :: Lens.Lens' PutOrganizationConfigRuleResponse Prelude.Int
putOrganizationConfigRuleResponse_httpStatus = Lens.lens (\PutOrganizationConfigRuleResponse' {httpStatus} -> httpStatus) (\s@PutOrganizationConfigRuleResponse' {} a -> s {httpStatus = a} :: PutOrganizationConfigRuleResponse)

instance
  Prelude.NFData
    PutOrganizationConfigRuleResponse
  where
  rnf PutOrganizationConfigRuleResponse' {..} =
    Prelude.rnf organizationConfigRuleArn
      `Prelude.seq` Prelude.rnf httpStatus
