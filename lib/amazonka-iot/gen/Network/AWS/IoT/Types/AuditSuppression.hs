{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditSuppression
  ( AuditSuppression (..),

    -- * Smart constructor
    mkAuditSuppression,

    -- * Lenses
    asCheckName,
    asResourceIdentifier,
    asDescription,
    asExpirationDate,
    asSuppressIndefinitely,
  )
where

import qualified Network.AWS.IoT.Types.CheckName as Types
import qualified Network.AWS.IoT.Types.Description as Types
import qualified Network.AWS.IoT.Types.ResourceIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters out specific findings of a Device Defender audit.
--
-- /See:/ 'mkAuditSuppression' smart constructor.
data AuditSuppression = AuditSuppression'
  { checkName :: Types.CheckName,
    resourceIdentifier :: Types.ResourceIdentifier,
    -- | The description of the audit suppression.
    description :: Core.Maybe Types.Description,
    -- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AuditSuppression' value with any optional fields omitted.
mkAuditSuppression ::
  -- | 'checkName'
  Types.CheckName ->
  -- | 'resourceIdentifier'
  Types.ResourceIdentifier ->
  AuditSuppression
mkAuditSuppression checkName resourceIdentifier =
  AuditSuppression'
    { checkName,
      resourceIdentifier,
      description = Core.Nothing,
      expirationDate = Core.Nothing,
      suppressIndefinitely = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCheckName :: Lens.Lens' AuditSuppression Types.CheckName
asCheckName = Lens.field @"checkName"
{-# DEPRECATED asCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asResourceIdentifier :: Lens.Lens' AuditSuppression Types.ResourceIdentifier
asResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED asResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The description of the audit suppression.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDescription :: Lens.Lens' AuditSuppression (Core.Maybe Types.Description)
asDescription = Lens.field @"description"
{-# DEPRECATED asDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The expiration date (epoch timestamp in seconds) that you want the suppression to adhere to.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asExpirationDate :: Lens.Lens' AuditSuppression (Core.Maybe Core.NominalDiffTime)
asExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED asExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Indicates whether a suppression should exist indefinitely or not.
--
-- /Note:/ Consider using 'suppressIndefinitely' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSuppressIndefinitely :: Lens.Lens' AuditSuppression (Core.Maybe Core.Bool)
asSuppressIndefinitely = Lens.field @"suppressIndefinitely"
{-# DEPRECATED asSuppressIndefinitely "Use generic-lens or generic-optics with 'suppressIndefinitely' instead." #-}

instance Core.FromJSON AuditSuppression where
  parseJSON =
    Core.withObject "AuditSuppression" Core.$
      \x ->
        AuditSuppression'
          Core.<$> (x Core..: "checkName")
          Core.<*> (x Core..: "resourceIdentifier")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "expirationDate")
          Core.<*> (x Core..:? "suppressIndefinitely")
