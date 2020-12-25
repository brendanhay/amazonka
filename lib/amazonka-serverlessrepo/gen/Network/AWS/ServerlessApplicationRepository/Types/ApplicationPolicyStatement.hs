{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
  ( ApplicationPolicyStatement (..),

    -- * Smart constructor
    mkApplicationPolicyStatement,

    -- * Lenses
    apsPrincipals,
    apsActions,
    apsPrincipalOrgIDs,
    apsStatementId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Policy statement applied to the application.
--
-- /See:/ 'mkApplicationPolicyStatement' smart constructor.
data ApplicationPolicyStatement = ApplicationPolicyStatement'
  { -- | An array of AWS account IDs, or * to make the application public.
    principals :: [Core.Text],
    -- | For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
    --
    --  Permissions> .
    actions :: [Core.Text],
    -- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
    principalOrgIDs :: Core.Maybe [Core.Text],
    -- | A unique ID for the statement.
    statementId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationPolicyStatement' value with any optional fields omitted.
mkApplicationPolicyStatement ::
  ApplicationPolicyStatement
mkApplicationPolicyStatement =
  ApplicationPolicyStatement'
    { principals = Core.mempty,
      actions = Core.mempty,
      principalOrgIDs = Core.Nothing,
      statementId = Core.Nothing
    }

-- | An array of AWS account IDs, or * to make the application public.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPrincipals :: Lens.Lens' ApplicationPolicyStatement [Core.Text]
apsPrincipals = Lens.field @"principals"
{-# DEPRECATED apsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
--
--  Permissions> .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsActions :: Lens.Lens' ApplicationPolicyStatement [Core.Text]
apsActions = Lens.field @"actions"
{-# DEPRECATED apsActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
--
-- /Note:/ Consider using 'principalOrgIDs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPrincipalOrgIDs :: Lens.Lens' ApplicationPolicyStatement (Core.Maybe [Core.Text])
apsPrincipalOrgIDs = Lens.field @"principalOrgIDs"
{-# DEPRECATED apsPrincipalOrgIDs "Use generic-lens or generic-optics with 'principalOrgIDs' instead." #-}

-- | A unique ID for the statement.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsStatementId :: Lens.Lens' ApplicationPolicyStatement (Core.Maybe Core.Text)
apsStatementId = Lens.field @"statementId"
{-# DEPRECATED apsStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

instance Core.FromJSON ApplicationPolicyStatement where
  toJSON ApplicationPolicyStatement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("principals" Core..= principals),
            Core.Just ("actions" Core..= actions),
            ("principalOrgIDs" Core..=) Core.<$> principalOrgIDs,
            ("statementId" Core..=) Core.<$> statementId
          ]
      )

instance Core.FromJSON ApplicationPolicyStatement where
  parseJSON =
    Core.withObject "ApplicationPolicyStatement" Core.$
      \x ->
        ApplicationPolicyStatement'
          Core.<$> (x Core..:? "principals" Core..!= Core.mempty)
          Core.<*> (x Core..:? "actions" Core..!= Core.mempty)
          Core.<*> (x Core..:? "principalOrgIDs")
          Core.<*> (x Core..:? "statementId")
