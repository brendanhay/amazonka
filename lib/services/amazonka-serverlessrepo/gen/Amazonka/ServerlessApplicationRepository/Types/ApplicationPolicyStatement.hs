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
-- Module      : Amazonka.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types.ApplicationPolicyStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Policy statement applied to the application.
--
-- /See:/ 'newApplicationPolicyStatement' smart constructor.
data ApplicationPolicyStatement = ApplicationPolicyStatement'
  { -- | A unique ID for the statement.
    statementId :: Prelude.Maybe Prelude.Text,
    -- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID>
    -- global condition key.
    principalOrgIDs :: Prelude.Maybe [Prelude.Text],
    -- | An array of AWS account IDs, or * to make the application public.
    principals :: [Prelude.Text],
    -- | For the list of actions supported for this operation, see
    -- <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application Permissions>.
    actions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      principalOrgIDs = Prelude.Nothing,
      principals = Prelude.mempty,
      actions = Prelude.mempty
    }

-- | A unique ID for the statement.
applicationPolicyStatement_statementId :: Lens.Lens' ApplicationPolicyStatement (Prelude.Maybe Prelude.Text)
applicationPolicyStatement_statementId = Lens.lens (\ApplicationPolicyStatement' {statementId} -> statementId) (\s@ApplicationPolicyStatement' {} a -> s {statementId = a} :: ApplicationPolicyStatement)

-- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID>
-- global condition key.
applicationPolicyStatement_principalOrgIDs :: Lens.Lens' ApplicationPolicyStatement (Prelude.Maybe [Prelude.Text])
applicationPolicyStatement_principalOrgIDs = Lens.lens (\ApplicationPolicyStatement' {principalOrgIDs} -> principalOrgIDs) (\s@ApplicationPolicyStatement' {} a -> s {principalOrgIDs = a} :: ApplicationPolicyStatement) Prelude.. Lens.mapping Lens.coerced

-- | An array of AWS account IDs, or * to make the application public.
applicationPolicyStatement_principals :: Lens.Lens' ApplicationPolicyStatement [Prelude.Text]
applicationPolicyStatement_principals = Lens.lens (\ApplicationPolicyStatement' {principals} -> principals) (\s@ApplicationPolicyStatement' {} a -> s {principals = a} :: ApplicationPolicyStatement) Prelude.. Lens.coerced

-- | For the list of actions supported for this operation, see
-- <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application Permissions>.
applicationPolicyStatement_actions :: Lens.Lens' ApplicationPolicyStatement [Prelude.Text]
applicationPolicyStatement_actions = Lens.lens (\ApplicationPolicyStatement' {actions} -> actions) (\s@ApplicationPolicyStatement' {} a -> s {actions = a} :: ApplicationPolicyStatement) Prelude.. Lens.coerced

instance Core.FromJSON ApplicationPolicyStatement where
  parseJSON =
    Core.withObject
      "ApplicationPolicyStatement"
      ( \x ->
          ApplicationPolicyStatement'
            Prelude.<$> (x Core..:? "statementId")
            Prelude.<*> ( x Core..:? "principalOrgIDs"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "principals" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "actions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ApplicationPolicyStatement

instance Prelude.NFData ApplicationPolicyStatement

instance Core.ToJSON ApplicationPolicyStatement where
  toJSON ApplicationPolicyStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("statementId" Core..=) Prelude.<$> statementId,
            ("principalOrgIDs" Core..=)
              Prelude.<$> principalOrgIDs,
            Prelude.Just ("principals" Core..= principals),
            Prelude.Just ("actions" Core..= actions)
          ]
      )
