{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SecurityConfigurationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SecurityConfigurationSummary
  ( SecurityConfigurationSummary (..),

    -- * Smart constructor
    mkSecurityConfigurationSummary,

    -- * Lenses
    scsCreationDateTime,
    scsName,
  )
where

import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The creation date and time, and name, of a security configuration.
--
-- /See:/ 'mkSecurityConfigurationSummary' smart constructor.
data SecurityConfigurationSummary = SecurityConfigurationSummary'
  { -- | The date and time the security configuration was created.
    creationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the security configuration.
    name :: Core.Maybe Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SecurityConfigurationSummary' value with any optional fields omitted.
mkSecurityConfigurationSummary ::
  SecurityConfigurationSummary
mkSecurityConfigurationSummary =
  SecurityConfigurationSummary'
    { creationDateTime = Core.Nothing,
      name = Core.Nothing
    }

-- | The date and time the security configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsCreationDateTime :: Lens.Lens' SecurityConfigurationSummary (Core.Maybe Core.NominalDiffTime)
scsCreationDateTime = Lens.field @"creationDateTime"
{-# DEPRECATED scsCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsName :: Lens.Lens' SecurityConfigurationSummary (Core.Maybe Types.XmlString)
scsName = Lens.field @"name"
{-# DEPRECATED scsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SecurityConfigurationSummary where
  parseJSON =
    Core.withObject "SecurityConfigurationSummary" Core.$
      \x ->
        SecurityConfigurationSummary'
          Core.<$> (x Core..:? "CreationDateTime") Core.<*> (x Core..:? "Name")
