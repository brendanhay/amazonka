{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HsmStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.HsmStatus
  ( HsmStatus (..)
  -- * Smart constructor
  , mkHsmStatus
  -- * Lenses
  , hsHsmClientCertificateIdentifier
  , hsHsmConfigurationIdentifier
  , hsStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes the status of changes to HSM settings.
--
-- /See:/ 'mkHsmStatus' smart constructor.
data HsmStatus = HsmStatus'
  { hsmClientCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
  , hsmConfigurationIdentifier :: Core.Maybe Core.Text
    -- ^ Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
  , status :: Core.Maybe Core.Text
    -- ^ Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HsmStatus' value with any optional fields omitted.
mkHsmStatus
    :: HsmStatus
mkHsmStatus
  = HsmStatus'{hsmClientCertificateIdentifier = Core.Nothing,
               hsmConfigurationIdentifier = Core.Nothing, status = Core.Nothing}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHsmClientCertificateIdentifier :: Lens.Lens' HsmStatus (Core.Maybe Core.Text)
hsHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE hsHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHsmConfigurationIdentifier :: Lens.Lens' HsmStatus (Core.Maybe Core.Text)
hsHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE hsHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsStatus :: Lens.Lens' HsmStatus (Core.Maybe Core.Text)
hsStatus = Lens.field @"status"
{-# INLINEABLE hsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML HsmStatus where
        parseXML x
          = HsmStatus' Core.<$>
              (x Core..@? "HsmClientCertificateIdentifier") Core.<*>
                x Core..@? "HsmConfigurationIdentifier"
                Core.<*> x Core..@? "Status"
