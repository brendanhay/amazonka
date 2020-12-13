{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.CodeSigningPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningPolicies
  ( CodeSigningPolicies (..),

    -- * Smart constructor
    mkCodeSigningPolicies,

    -- * Lenses
    cspUntrustedArtifactOnDeployment,
  )
where

import Network.AWS.Lambda.Types.CodeSigningPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Code signing configuration policies specifies the validation failure action for signature mismatch or expiry.
--
-- /See:/ 'mkCodeSigningPolicies' smart constructor.
newtype CodeSigningPolicies = CodeSigningPolicies'
  { -- | Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.
    --
    -- Default value: @Warn@
    untrustedArtifactOnDeployment :: Lude.Maybe CodeSigningPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeSigningPolicies' with the minimum fields required to make a request.
--
-- * 'untrustedArtifactOnDeployment' - Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.
--
-- Default value: @Warn@
mkCodeSigningPolicies ::
  CodeSigningPolicies
mkCodeSigningPolicies =
  CodeSigningPolicies'
    { untrustedArtifactOnDeployment =
        Lude.Nothing
    }

-- | Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.
--
-- Default value: @Warn@
--
-- /Note:/ Consider using 'untrustedArtifactOnDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspUntrustedArtifactOnDeployment :: Lens.Lens' CodeSigningPolicies (Lude.Maybe CodeSigningPolicy)
cspUntrustedArtifactOnDeployment = Lens.lens (untrustedArtifactOnDeployment :: CodeSigningPolicies -> Lude.Maybe CodeSigningPolicy) (\s a -> s {untrustedArtifactOnDeployment = a} :: CodeSigningPolicies)
{-# DEPRECATED cspUntrustedArtifactOnDeployment "Use generic-lens or generic-optics with 'untrustedArtifactOnDeployment' instead." #-}

instance Lude.FromJSON CodeSigningPolicies where
  parseJSON =
    Lude.withObject
      "CodeSigningPolicies"
      ( \x ->
          CodeSigningPolicies'
            Lude.<$> (x Lude..:? "UntrustedArtifactOnDeployment")
      )

instance Lude.ToJSON CodeSigningPolicies where
  toJSON CodeSigningPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UntrustedArtifactOnDeployment" Lude..=)
              Lude.<$> untrustedArtifactOnDeployment
          ]
      )
