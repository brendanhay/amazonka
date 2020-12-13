{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Organization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Organization
  ( Organization (..),

    -- * Smart constructor
    mkOrganization,

    -- * Lenses
    oOrg,
    oASNOrg,
    oASN,
    oIsp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the ISP organization of the remote IP address.
--
-- /See:/ 'mkOrganization' smart constructor.
data Organization = Organization'
  { -- | The name of the internet provider.
    org :: Lude.Maybe Lude.Text,
    -- | The organization that registered this ASN.
    asnOrg :: Lude.Maybe Lude.Text,
    -- | The Autonomous System Number (ASN) of the internet provider of the remote IP address.
    asn :: Lude.Maybe Lude.Text,
    -- | The ISP information for the internet provider.
    isp :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Organization' with the minimum fields required to make a request.
--
-- * 'org' - The name of the internet provider.
-- * 'asnOrg' - The organization that registered this ASN.
-- * 'asn' - The Autonomous System Number (ASN) of the internet provider of the remote IP address.
-- * 'isp' - The ISP information for the internet provider.
mkOrganization ::
  Organization
mkOrganization =
  Organization'
    { org = Lude.Nothing,
      asnOrg = Lude.Nothing,
      asn = Lude.Nothing,
      isp = Lude.Nothing
    }

-- | The name of the internet provider.
--
-- /Note:/ Consider using 'org' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOrg :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oOrg = Lens.lens (org :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {org = a} :: Organization)
{-# DEPRECATED oOrg "Use generic-lens or generic-optics with 'org' instead." #-}

-- | The organization that registered this ASN.
--
-- /Note:/ Consider using 'asnOrg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oASNOrg :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oASNOrg = Lens.lens (asnOrg :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {asnOrg = a} :: Organization)
{-# DEPRECATED oASNOrg "Use generic-lens or generic-optics with 'asnOrg' instead." #-}

-- | The Autonomous System Number (ASN) of the internet provider of the remote IP address.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oASN :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oASN = Lens.lens (asn :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {asn = a} :: Organization)
{-# DEPRECATED oASN "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The ISP information for the internet provider.
--
-- /Note:/ Consider using 'isp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oIsp :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oIsp = Lens.lens (isp :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {isp = a} :: Organization)
{-# DEPRECATED oIsp "Use generic-lens or generic-optics with 'isp' instead." #-}

instance Lude.FromJSON Organization where
  parseJSON =
    Lude.withObject
      "Organization"
      ( \x ->
          Organization'
            Lude.<$> (x Lude..:? "org")
            Lude.<*> (x Lude..:? "asnOrg")
            Lude.<*> (x Lude..:? "asn")
            Lude.<*> (x Lude..:? "isp")
      )
