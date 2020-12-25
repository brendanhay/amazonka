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

import qualified Network.AWS.Lambda.Types.CodeSigningPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Code signing configuration policies specifies the validation failure action for signature mismatch or expiry.
--
-- /See:/ 'mkCodeSigningPolicies' smart constructor.
newtype CodeSigningPolicies = CodeSigningPolicies'
  { -- | Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.
    --
    -- Default value: @Warn@
    untrustedArtifactOnDeployment :: Core.Maybe Types.CodeSigningPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CodeSigningPolicies' value with any optional fields omitted.
mkCodeSigningPolicies ::
  CodeSigningPolicies
mkCodeSigningPolicies =
  CodeSigningPolicies'
    { untrustedArtifactOnDeployment =
        Core.Nothing
    }

-- | Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.
--
-- Default value: @Warn@
--
-- /Note:/ Consider using 'untrustedArtifactOnDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspUntrustedArtifactOnDeployment :: Lens.Lens' CodeSigningPolicies (Core.Maybe Types.CodeSigningPolicy)
cspUntrustedArtifactOnDeployment = Lens.field @"untrustedArtifactOnDeployment"
{-# DEPRECATED cspUntrustedArtifactOnDeployment "Use generic-lens or generic-optics with 'untrustedArtifactOnDeployment' instead." #-}

instance Core.FromJSON CodeSigningPolicies where
  toJSON CodeSigningPolicies {..} =
    Core.object
      ( Core.catMaybes
          [ ("UntrustedArtifactOnDeployment" Core..=)
              Core.<$> untrustedArtifactOnDeployment
          ]
      )

instance Core.FromJSON CodeSigningPolicies where
  parseJSON =
    Core.withObject "CodeSigningPolicies" Core.$
      \x ->
        CodeSigningPolicies'
          Core.<$> (x Core..:? "UntrustedArtifactOnDeployment")
