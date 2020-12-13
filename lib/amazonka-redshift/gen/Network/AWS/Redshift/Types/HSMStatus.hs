{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HSMStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HSMStatus
  ( HSMStatus (..),

    -- * Smart constructor
    mkHSMStatus,

    -- * Lenses
    hsStatus,
    hsHSMConfigurationIdentifier,
    hsHSMClientCertificateIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the status of changes to HSM settings.
--
-- /See:/ 'mkHSMStatus' smart constructor.
data HSMStatus = HSMStatus'
  { -- | Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command.
    --
    -- Values: active, applying
    status :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    hsmConfigurationIdentifier :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HSMStatus' with the minimum fields required to make a request.
--
-- * 'status' - Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
-- * 'hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
-- * 'hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
mkHSMStatus ::
  HSMStatus
mkHSMStatus =
  HSMStatus'
    { status = Lude.Nothing,
      hsmConfigurationIdentifier = Lude.Nothing,
      hsmClientCertificateIdentifier = Lude.Nothing
    }

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsStatus :: Lens.Lens' HSMStatus (Lude.Maybe Lude.Text)
hsStatus = Lens.lens (status :: HSMStatus -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: HSMStatus)
{-# DEPRECATED hsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the name of the HSM configuration that contains the information the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHSMConfigurationIdentifier :: Lens.Lens' HSMStatus (Lude.Maybe Lude.Text)
hsHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: HSMStatus -> Lude.Maybe Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: HSMStatus)
{-# DEPRECATED hsHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsHSMClientCertificateIdentifier :: Lens.Lens' HSMStatus (Lude.Maybe Lude.Text)
hsHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: HSMStatus -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: HSMStatus)
{-# DEPRECATED hsHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

instance Lude.FromXML HSMStatus where
  parseXML x =
    HSMStatus'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "HsmConfigurationIdentifier")
      Lude.<*> (x Lude..@? "HsmClientCertificateIdentifier")
