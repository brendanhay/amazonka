{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Policy statement applied to the application.
--
-- /See:/ 'newApplicationPolicyStatement' smart constructor.
data ApplicationPolicyStatement = ApplicationPolicyStatement'
  { -- | A unique ID for the statement.
    statementId :: Core.Maybe Core.Text,
    -- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID>
    -- global condition key.
    principalOrgIDs :: Core.Maybe [Core.Text],
    -- | An array of AWS account IDs, or * to make the application public.
    principals :: [Core.Text],
    -- | For the list of actions supported for this operation, see
    -- <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application Permissions>.
    actions :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationPolicyStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statementId', 'applicationPolicyStatement_statementId' - A unique ID for the statement.
--
-- 'principalOrgIDs', 'applicationPolicyStatement_principalOrgIDs' - An array of PrinciplalOrgIDs, which corresponds to AWS IAM
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID>
-- global condition key.
--
-- 'principals', 'applicationPolicyStatement_principals' - An array of AWS account IDs, or * to make the application public.
--
-- 'actions', 'applicationPolicyStatement_actions' - For the list of actions supported for this operation, see
-- <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application Permissions>.
newApplicationPolicyStatement ::
  ApplicationPolicyStatement
newApplicationPolicyStatement =
  ApplicationPolicyStatement'
    { statementId =
        Core.Nothing,
      principalOrgIDs = Core.Nothing,
      principals = Core.mempty,
      actions = Core.mempty
    }

-- | A unique ID for the statement.
applicationPolicyStatement_statementId :: Lens.Lens' ApplicationPolicyStatement (Core.Maybe Core.Text)
applicationPolicyStatement_statementId = Lens.lens (\ApplicationPolicyStatement' {statementId} -> statementId) (\s@ApplicationPolicyStatement' {} a -> s {statementId = a} :: ApplicationPolicyStatement)

-- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID>
-- global condition key.
applicationPolicyStatement_principalOrgIDs :: Lens.Lens' ApplicationPolicyStatement (Core.Maybe [Core.Text])
applicationPolicyStatement_principalOrgIDs = Lens.lens (\ApplicationPolicyStatement' {principalOrgIDs} -> principalOrgIDs) (\s@ApplicationPolicyStatement' {} a -> s {principalOrgIDs = a} :: ApplicationPolicyStatement) Core.. Lens.mapping Lens._Coerce

-- | An array of AWS account IDs, or * to make the application public.
applicationPolicyStatement_principals :: Lens.Lens' ApplicationPolicyStatement [Core.Text]
applicationPolicyStatement_principals = Lens.lens (\ApplicationPolicyStatement' {principals} -> principals) (\s@ApplicationPolicyStatement' {} a -> s {principals = a} :: ApplicationPolicyStatement) Core.. Lens._Coerce

-- | For the list of actions supported for this operation, see
-- <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application Permissions>.
applicationPolicyStatement_actions :: Lens.Lens' ApplicationPolicyStatement [Core.Text]
applicationPolicyStatement_actions = Lens.lens (\ApplicationPolicyStatement' {actions} -> actions) (\s@ApplicationPolicyStatement' {} a -> s {actions = a} :: ApplicationPolicyStatement) Core.. Lens._Coerce

instance Core.FromJSON ApplicationPolicyStatement where
  parseJSON =
    Core.withObject
      "ApplicationPolicyStatement"
      ( \x ->
          ApplicationPolicyStatement'
            Core.<$> (x Core..:? "statementId")
            Core.<*> (x Core..:? "principalOrgIDs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "principals" Core..!= Core.mempty)
            Core.<*> (x Core..:? "actions" Core..!= Core.mempty)
      )

instance Core.Hashable ApplicationPolicyStatement

instance Core.NFData ApplicationPolicyStatement

instance Core.ToJSON ApplicationPolicyStatement where
  toJSON ApplicationPolicyStatement' {..} =
    Core.object
      ( Core.catMaybes
          [ ("statementId" Core..=) Core.<$> statementId,
            ("principalOrgIDs" Core..=) Core.<$> principalOrgIDs,
            Core.Just ("principals" Core..= principals),
            Core.Just ("actions" Core..= actions)
          ]
      )
