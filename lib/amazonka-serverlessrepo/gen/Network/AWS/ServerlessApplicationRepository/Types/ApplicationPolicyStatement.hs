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
    apsActions,
    apsPrincipals,
    apsStatementId,
    apsPrincipalOrgIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Policy statement applied to the application.
--
-- /See:/ 'mkApplicationPolicyStatement' smart constructor.
data ApplicationPolicyStatement = ApplicationPolicyStatement'
  { -- | For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
    --
    --  Permissions> .
    actions :: [Lude.Text],
    -- | An array of AWS account IDs, or * to make the application public.
    principals :: [Lude.Text],
    -- | A unique ID for the statement.
    statementId :: Lude.Maybe Lude.Text,
    -- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
    principalOrgIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationPolicyStatement' with the minimum fields required to make a request.
--
-- * 'actions' - For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
--
--  Permissions> .
-- * 'principals' - An array of AWS account IDs, or * to make the application public.
-- * 'statementId' - A unique ID for the statement.
-- * 'principalOrgIds' - An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
mkApplicationPolicyStatement ::
  ApplicationPolicyStatement
mkApplicationPolicyStatement =
  ApplicationPolicyStatement'
    { actions = Lude.mempty,
      principals = Lude.mempty,
      statementId = Lude.Nothing,
      principalOrgIds = Lude.Nothing
    }

-- | For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application
--
--  Permissions> .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsActions :: Lens.Lens' ApplicationPolicyStatement [Lude.Text]
apsActions = Lens.lens (actions :: ApplicationPolicyStatement -> [Lude.Text]) (\s a -> s {actions = a} :: ApplicationPolicyStatement)
{-# DEPRECATED apsActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | An array of AWS account IDs, or * to make the application public.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPrincipals :: Lens.Lens' ApplicationPolicyStatement [Lude.Text]
apsPrincipals = Lens.lens (principals :: ApplicationPolicyStatement -> [Lude.Text]) (\s a -> s {principals = a} :: ApplicationPolicyStatement)
{-# DEPRECATED apsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | A unique ID for the statement.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsStatementId :: Lens.Lens' ApplicationPolicyStatement (Lude.Maybe Lude.Text)
apsStatementId = Lens.lens (statementId :: ApplicationPolicyStatement -> Lude.Maybe Lude.Text) (\s a -> s {statementId = a} :: ApplicationPolicyStatement)
{-# DEPRECATED apsStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
--
-- /Note:/ Consider using 'principalOrgIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPrincipalOrgIds :: Lens.Lens' ApplicationPolicyStatement (Lude.Maybe [Lude.Text])
apsPrincipalOrgIds = Lens.lens (principalOrgIds :: ApplicationPolicyStatement -> Lude.Maybe [Lude.Text]) (\s a -> s {principalOrgIds = a} :: ApplicationPolicyStatement)
{-# DEPRECATED apsPrincipalOrgIds "Use generic-lens or generic-optics with 'principalOrgIds' instead." #-}

instance Lude.FromJSON ApplicationPolicyStatement where
  parseJSON =
    Lude.withObject
      "ApplicationPolicyStatement"
      ( \x ->
          ApplicationPolicyStatement'
            Lude.<$> (x Lude..:? "actions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "principals" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statementId")
            Lude.<*> (x Lude..:? "principalOrgIDs" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ApplicationPolicyStatement where
  toJSON ApplicationPolicyStatement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("actions" Lude..= actions),
            Lude.Just ("principals" Lude..= principals),
            ("statementId" Lude..=) Lude.<$> statementId,
            ("principalOrgIDs" Lude..=) Lude.<$> principalOrgIds
          ]
      )
