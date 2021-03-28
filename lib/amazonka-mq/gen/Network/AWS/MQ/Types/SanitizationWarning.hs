{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.SanitizationWarning
  ( SanitizationWarning (..)
  -- * Smart constructor
  , mkSanitizationWarning
  -- * Lenses
  , swAttributeName
  , swElementName
  , swReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.SanitizationWarningReason as Types
import qualified Network.AWS.Prelude as Core

-- | Returns information about the XML element or attribute that was sanitized in the configuration.
--
-- /See:/ 'mkSanitizationWarning' smart constructor.
data SanitizationWarning = SanitizationWarning'
  { attributeName :: Core.Maybe Core.Text
    -- ^ The name of the XML attribute that has been sanitized.
  , elementName :: Core.Maybe Core.Text
    -- ^ The name of the XML element that has been sanitized.
  , reason :: Core.Maybe Types.SanitizationWarningReason
    -- ^ Required. The reason for which the XML elements or attributes were sanitized.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SanitizationWarning' value with any optional fields omitted.
mkSanitizationWarning
    :: SanitizationWarning
mkSanitizationWarning
  = SanitizationWarning'{attributeName = Core.Nothing,
                         elementName = Core.Nothing, reason = Core.Nothing}

-- | The name of the XML attribute that has been sanitized.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swAttributeName :: Lens.Lens' SanitizationWarning (Core.Maybe Core.Text)
swAttributeName = Lens.field @"attributeName"
{-# INLINEABLE swAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The name of the XML element that has been sanitized.
--
-- /Note:/ Consider using 'elementName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swElementName :: Lens.Lens' SanitizationWarning (Core.Maybe Core.Text)
swElementName = Lens.field @"elementName"
{-# INLINEABLE swElementName #-}
{-# DEPRECATED elementName "Use generic-lens or generic-optics with 'elementName' instead"  #-}

-- | Required. The reason for which the XML elements or attributes were sanitized.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swReason :: Lens.Lens' SanitizationWarning (Core.Maybe Types.SanitizationWarningReason)
swReason = Lens.field @"reason"
{-# INLINEABLE swReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.FromJSON SanitizationWarning where
        parseJSON
          = Core.withObject "SanitizationWarning" Core.$
              \ x ->
                SanitizationWarning' Core.<$>
                  (x Core..:? "attributeName") Core.<*> x Core..:? "elementName"
                    Core.<*> x Core..:? "reason"
