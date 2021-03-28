{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.UpdateCACertificateParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.UpdateCACertificateParams
  ( UpdateCACertificateParams (..)
  -- * Smart constructor
  , mkUpdateCACertificateParams
  -- * Lenses
  , ucacpAction
  ) where

import qualified Network.AWS.IoT.Types.CACertificateUpdateAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
--
-- /See:/ 'mkUpdateCACertificateParams' smart constructor.
newtype UpdateCACertificateParams = UpdateCACertificateParams'
  { action :: Types.CACertificateUpdateAction
    -- ^ The action that you want to apply to the CA cerrtificate. The only supported value is @DEACTIVATE@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCACertificateParams' value with any optional fields omitted.
mkUpdateCACertificateParams
    :: Types.CACertificateUpdateAction -- ^ 'action'
    -> UpdateCACertificateParams
mkUpdateCACertificateParams action
  = UpdateCACertificateParams'{action}

-- | The action that you want to apply to the CA cerrtificate. The only supported value is @DEACTIVATE@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacpAction :: Lens.Lens' UpdateCACertificateParams Types.CACertificateUpdateAction
ucacpAction = Lens.field @"action"
{-# INLINEABLE ucacpAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

instance Core.FromJSON UpdateCACertificateParams where
        toJSON UpdateCACertificateParams{..}
          = Core.object
              (Core.catMaybes [Core.Just ("action" Core..= action)])

instance Core.FromJSON UpdateCACertificateParams where
        parseJSON
          = Core.withObject "UpdateCACertificateParams" Core.$
              \ x -> UpdateCACertificateParams' Core.<$> (x Core..: "action")
