{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SecurityConfiguration
  ( SecurityConfiguration (..),

    -- * Smart constructor
    mkSecurityConfiguration,

    -- * Lenses
    sCreatedTimeStamp,
    sEncryptionConfiguration,
    sName,
  )
where

import qualified Network.AWS.Glue.Types.EncryptionConfiguration as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a security configuration.
--
-- /See:/ 'mkSecurityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { -- | The time at which this security configuration was created.
    createdTimeStamp :: Core.Maybe Core.NominalDiffTime,
    -- | The encryption configuration associated with this security configuration.
    encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration,
    -- | The name of the security configuration.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SecurityConfiguration' value with any optional fields omitted.
mkSecurityConfiguration ::
  SecurityConfiguration
mkSecurityConfiguration =
  SecurityConfiguration'
    { createdTimeStamp = Core.Nothing,
      encryptionConfiguration = Core.Nothing,
      name = Core.Nothing
    }

-- | The time at which this security configuration was created.
--
-- /Note:/ Consider using 'createdTimeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedTimeStamp :: Lens.Lens' SecurityConfiguration (Core.Maybe Core.NominalDiffTime)
sCreatedTimeStamp = Lens.field @"createdTimeStamp"
{-# DEPRECATED sCreatedTimeStamp "Use generic-lens or generic-optics with 'createdTimeStamp' instead." #-}

-- | The encryption configuration associated with this security configuration.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEncryptionConfiguration :: Lens.Lens' SecurityConfiguration (Core.Maybe Types.EncryptionConfiguration)
sEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# DEPRECATED sEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' SecurityConfiguration (Core.Maybe Types.Name)
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SecurityConfiguration where
  parseJSON =
    Core.withObject "SecurityConfiguration" Core.$
      \x ->
        SecurityConfiguration'
          Core.<$> (x Core..:? "CreatedTimeStamp")
          Core.<*> (x Core..:? "EncryptionConfiguration")
          Core.<*> (x Core..:? "Name")
