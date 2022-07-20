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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates organization config rule for your entire organization
-- evaluating whether your Amazon Web Services resources comply with your
-- desired configurations.
--
-- Only a master account and a delegated administrator can create or update
-- an organization config rule. When calling this API with a delegated
-- administrator, you must ensure Organizations
-- @ListDelegatedAdministrator@ permissions are added.
--
-- This API enables organization service access through the
-- @EnableAWSServiceAccess@ action and creates a service linked role
-- @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated
-- administrator account of your organization. The service linked role is
-- created only when the role does not exist in the caller account. Config
-- verifies the existence of role with @GetRole@ action.
--
-- To use this API with delegated administrator, register a delegated
-- administrator by calling Amazon Web Services Organization
-- @register-delegated-administrator@ for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- You can use this action to create both custom Config rules and Config
-- managed rules. If you are adding a new custom Config rule, you must
-- first create Lambda function in the master account or a delegated
-- administrator that the rule invokes to evaluate your resources. You also
-- need to create an IAM role in the managed-account that can be assumed by
-- the Lambda function. When you use the @PutOrganizationConfigRule@ action
-- to add the rule to Config, you must specify the Amazon Resource Name
-- (ARN) that Lambda assigns to the function. If you are adding an Config
-- managed rule, specify the rule\'s identifier for the @RuleIdentifier@
-- key.
--
-- The maximum number of organization config rules that Config supports is
-- 150 and 3 delegated administrator per organization.
--
-- Prerequisite: Ensure you call @EnableAllFeatures@ API to enable all
-- features in an organization.
--
-- Specify either @OrganizationCustomRuleMetadata@ or
-- @OrganizationManagedRuleMetadata@.
module Amazonka.Config.PutOrganizationConfigRule
  ( -- * Creating a Request
    PutOrganizationConfigRule (..),
    newPutOrganizationConfigRule,

    -- * Request Lenses
    putOrganizationConfigRule_excludedAccounts,
    putOrganizationConfigRule_organizationManagedRuleMetadata,
    putOrganizationConfigRule_organizationCustomRuleMetadata,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutOrganizationConfigRule' smart constructor.
data PutOrganizationConfigRule = PutOrganizationConfigRule'
  { -- | A comma-separated list of accounts that you want to exclude from an
    -- organization config rule.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | An @OrganizationManagedRuleMetadata@ object.
    organizationManagedRuleMetadata :: Prelude.Maybe OrganizationManagedRuleMetadata,
    -- | An @OrganizationCustomRuleMetadata@ object.
    organizationCustomRuleMetadata :: Prelude.Maybe OrganizationCustomRuleMetadata,
    -- | The name that you assign to an organization config rule.
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
-- organization config rule.
--
-- 'organizationManagedRuleMetadata', 'putOrganizationConfigRule_organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
--
-- 'organizationCustomRuleMetadata', 'putOrganizationConfigRule_organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
--
-- 'organizationConfigRuleName', 'putOrganizationConfigRule_organizationConfigRuleName' - The name that you assign to an organization config rule.
newPutOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  PutOrganizationConfigRule
newPutOrganizationConfigRule
  pOrganizationConfigRuleName_ =
    PutOrganizationConfigRule'
      { excludedAccounts =
          Prelude.Nothing,
        organizationManagedRuleMetadata =
          Prelude.Nothing,
        organizationCustomRuleMetadata = Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | A comma-separated list of accounts that you want to exclude from an
-- organization config rule.
putOrganizationConfigRule_excludedAccounts :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe [Prelude.Text])
putOrganizationConfigRule_excludedAccounts = Lens.lens (\PutOrganizationConfigRule' {excludedAccounts} -> excludedAccounts) (\s@PutOrganizationConfigRule' {} a -> s {excludedAccounts = a} :: PutOrganizationConfigRule) Prelude.. Lens.mapping Lens.coerced

-- | An @OrganizationManagedRuleMetadata@ object.
putOrganizationConfigRule_organizationManagedRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe OrganizationManagedRuleMetadata)
putOrganizationConfigRule_organizationManagedRuleMetadata = Lens.lens (\PutOrganizationConfigRule' {organizationManagedRuleMetadata} -> organizationManagedRuleMetadata) (\s@PutOrganizationConfigRule' {} a -> s {organizationManagedRuleMetadata = a} :: PutOrganizationConfigRule)

-- | An @OrganizationCustomRuleMetadata@ object.
putOrganizationConfigRule_organizationCustomRuleMetadata :: Lens.Lens' PutOrganizationConfigRule (Prelude.Maybe OrganizationCustomRuleMetadata)
putOrganizationConfigRule_organizationCustomRuleMetadata = Lens.lens (\PutOrganizationConfigRule' {organizationCustomRuleMetadata} -> organizationCustomRuleMetadata) (\s@PutOrganizationConfigRule' {} a -> s {organizationCustomRuleMetadata = a} :: PutOrganizationConfigRule)

-- | The name that you assign to an organization config rule.
putOrganizationConfigRule_organizationConfigRuleName :: Lens.Lens' PutOrganizationConfigRule Prelude.Text
putOrganizationConfigRule_organizationConfigRuleName = Lens.lens (\PutOrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@PutOrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: PutOrganizationConfigRule)

instance Core.AWSRequest PutOrganizationConfigRule where
  type
    AWSResponse PutOrganizationConfigRule =
      PutOrganizationConfigRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutOrganizationConfigRuleResponse'
            Prelude.<$> (x Core..?> "OrganizationConfigRuleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutOrganizationConfigRule where
  hashWithSalt _salt PutOrganizationConfigRule' {..} =
    _salt `Prelude.hashWithSalt` excludedAccounts
      `Prelude.hashWithSalt` organizationManagedRuleMetadata
      `Prelude.hashWithSalt` organizationCustomRuleMetadata
      `Prelude.hashWithSalt` organizationConfigRuleName

instance Prelude.NFData PutOrganizationConfigRule where
  rnf PutOrganizationConfigRule' {..} =
    Prelude.rnf excludedAccounts
      `Prelude.seq` Prelude.rnf organizationManagedRuleMetadata
      `Prelude.seq` Prelude.rnf organizationCustomRuleMetadata
      `Prelude.seq` Prelude.rnf organizationConfigRuleName

instance Core.ToHeaders PutOrganizationConfigRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutOrganizationConfigRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutOrganizationConfigRule where
  toJSON PutOrganizationConfigRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExcludedAccounts" Core..=)
              Prelude.<$> excludedAccounts,
            ("OrganizationManagedRuleMetadata" Core..=)
              Prelude.<$> organizationManagedRuleMetadata,
            ("OrganizationCustomRuleMetadata" Core..=)
              Prelude.<$> organizationCustomRuleMetadata,
            Prelude.Just
              ( "OrganizationConfigRuleName"
                  Core..= organizationConfigRuleName
              )
          ]
      )

instance Core.ToPath PutOrganizationConfigRule where
  toPath = Prelude.const "/"

instance Core.ToQuery PutOrganizationConfigRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutOrganizationConfigRuleResponse' smart constructor.
data PutOrganizationConfigRuleResponse = PutOrganizationConfigRuleResponse'
  { -- | The Amazon Resource Name (ARN) of an organization config rule.
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
-- 'organizationConfigRuleArn', 'putOrganizationConfigRuleResponse_organizationConfigRuleArn' - The Amazon Resource Name (ARN) of an organization config rule.
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

-- | The Amazon Resource Name (ARN) of an organization config rule.
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
