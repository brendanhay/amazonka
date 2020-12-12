{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DomainDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DomainDetails
  ( DomainDetails (..),

    -- * Smart constructor
    mkDomainDetails,

    -- * Lenses
    ddDomain,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the domain.
--
-- /See:/ 'mkDomainDetails' smart constructor.
newtype DomainDetails = DomainDetails'
  { domain ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainDetails' with the minimum fields required to make a request.
--
-- * 'domain' - The domain information for the AWS API call.
mkDomainDetails ::
  DomainDetails
mkDomainDetails = DomainDetails' {domain = Lude.Nothing}

-- | The domain information for the AWS API call.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomain :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Text)
ddDomain = Lens.lens (domain :: DomainDetails -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: DomainDetails)
{-# DEPRECATED ddDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.FromJSON DomainDetails where
  parseJSON =
    Lude.withObject
      "DomainDetails"
      (\x -> DomainDetails' Lude.<$> (x Lude..:? "domain"))
