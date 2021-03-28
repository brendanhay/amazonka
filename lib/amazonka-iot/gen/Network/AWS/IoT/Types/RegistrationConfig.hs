{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RegistrationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.RegistrationConfig
  ( RegistrationConfig (..)
  -- * Smart constructor
  , mkRegistrationConfig
  -- * Lenses
  , rcRoleArn
  , rcTemplateBody
  ) where

import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.IoT.Types.TemplateBody as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The registration configuration.
--
-- /See:/ 'mkRegistrationConfig' smart constructor.
data RegistrationConfig = RegistrationConfig'
  { roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the role.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The template body.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegistrationConfig' value with any optional fields omitted.
mkRegistrationConfig
    :: RegistrationConfig
mkRegistrationConfig
  = RegistrationConfig'{roleArn = Core.Nothing,
                        templateBody = Core.Nothing}

-- | The ARN of the role.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRoleArn :: Lens.Lens' RegistrationConfig (Core.Maybe Types.RoleArn)
rcRoleArn = Lens.field @"roleArn"
{-# INLINEABLE rcRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The template body.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTemplateBody :: Lens.Lens' RegistrationConfig (Core.Maybe Types.TemplateBody)
rcTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE rcTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

instance Core.FromJSON RegistrationConfig where
        toJSON RegistrationConfig{..}
          = Core.object
              (Core.catMaybes
                 [("roleArn" Core..=) Core.<$> roleArn,
                  ("templateBody" Core..=) Core.<$> templateBody])

instance Core.FromJSON RegistrationConfig where
        parseJSON
          = Core.withObject "RegistrationConfig" Core.$
              \ x ->
                RegistrationConfig' Core.<$>
                  (x Core..:? "roleArn") Core.<*> x Core..:? "templateBody"
