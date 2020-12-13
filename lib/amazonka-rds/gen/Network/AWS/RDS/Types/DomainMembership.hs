{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DomainMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DomainMembership
  ( DomainMembership (..),

    -- * Smart constructor
    mkDomainMembership,

    -- * Lenses
    dmStatus,
    dmFQDN,
    dmDomain,
    dmIAMRoleName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An Active Directory Domain membership record associated with the DB instance or cluster.
--
-- /See:/ 'mkDomainMembership' smart constructor.
data DomainMembership = DomainMembership'
  { -- | The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
    status :: Lude.Maybe Lude.Text,
    -- | The fully qualified domain name of the Active Directory Domain.
    fQDN :: Lude.Maybe Lude.Text,
    -- | The identifier of the Active Directory Domain.
    domain :: Lude.Maybe Lude.Text,
    -- | The name of the IAM role to be used when making API calls to the Directory Service.
    iamRoleName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainMembership' with the minimum fields required to make a request.
--
-- * 'status' - The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
-- * 'fQDN' - The fully qualified domain name of the Active Directory Domain.
-- * 'domain' - The identifier of the Active Directory Domain.
-- * 'iamRoleName' - The name of the IAM role to be used when making API calls to the Directory Service.
mkDomainMembership ::
  DomainMembership
mkDomainMembership =
  DomainMembership'
    { status = Lude.Nothing,
      fQDN = Lude.Nothing,
      domain = Lude.Nothing,
      iamRoleName = Lude.Nothing
    }

-- | The status of the Active Directory Domain membership for the DB instance or cluster. Values include joined, pending-join, failed, and so on.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmStatus :: Lens.Lens' DomainMembership (Lude.Maybe Lude.Text)
dmStatus = Lens.lens (status :: DomainMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DomainMembership)
{-# DEPRECATED dmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The fully qualified domain name of the Active Directory Domain.
--
-- /Note:/ Consider using 'fQDN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmFQDN :: Lens.Lens' DomainMembership (Lude.Maybe Lude.Text)
dmFQDN = Lens.lens (fQDN :: DomainMembership -> Lude.Maybe Lude.Text) (\s a -> s {fQDN = a} :: DomainMembership)
{-# DEPRECATED dmFQDN "Use generic-lens or generic-optics with 'fQDN' instead." #-}

-- | The identifier of the Active Directory Domain.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDomain :: Lens.Lens' DomainMembership (Lude.Maybe Lude.Text)
dmDomain = Lens.lens (domain :: DomainMembership -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: DomainMembership)
{-# DEPRECATED dmDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'iamRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmIAMRoleName :: Lens.Lens' DomainMembership (Lude.Maybe Lude.Text)
dmIAMRoleName = Lens.lens (iamRoleName :: DomainMembership -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleName = a} :: DomainMembership)
{-# DEPRECATED dmIAMRoleName "Use generic-lens or generic-optics with 'iamRoleName' instead." #-}

instance Lude.FromXML DomainMembership where
  parseXML x =
    DomainMembership'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "FQDN")
      Lude.<*> (x Lude..@? "Domain")
      Lude.<*> (x Lude..@? "IAMRoleName")
