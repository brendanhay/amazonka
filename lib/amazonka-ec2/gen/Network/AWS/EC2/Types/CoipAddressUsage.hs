{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CoipAddressUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CoipAddressUsage
  ( CoipAddressUsage (..),

    -- * Smart constructor
    mkCoipAddressUsage,

    -- * Lenses
    cauAllocationId,
    cauAWSAccountId,
    cauCoIP,
    cauAWSService,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes address usage for a customer-owned address pool.
--
-- /See:/ 'mkCoipAddressUsage' smart constructor.
data CoipAddressUsage = CoipAddressUsage'
  { allocationId ::
      Lude.Maybe Lude.Text,
    awsAccountId :: Lude.Maybe Lude.Text,
    coIP :: Lude.Maybe Lude.Text,
    awsService :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoipAddressUsage' with the minimum fields required to make a request.
--
-- * 'allocationId' - The allocation ID of the address.
-- * 'awsAccountId' - The AWS account ID.
-- * 'awsService' - The AWS service.
-- * 'coIP' - The customer-owned IP address.
mkCoipAddressUsage ::
  CoipAddressUsage
mkCoipAddressUsage =
  CoipAddressUsage'
    { allocationId = Lude.Nothing,
      awsAccountId = Lude.Nothing,
      coIP = Lude.Nothing,
      awsService = Lude.Nothing
    }

-- | The allocation ID of the address.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauAllocationId :: Lens.Lens' CoipAddressUsage (Lude.Maybe Lude.Text)
cauAllocationId = Lens.lens (allocationId :: CoipAddressUsage -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: CoipAddressUsage)
{-# DEPRECATED cauAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauAWSAccountId :: Lens.Lens' CoipAddressUsage (Lude.Maybe Lude.Text)
cauAWSAccountId = Lens.lens (awsAccountId :: CoipAddressUsage -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: CoipAddressUsage)
{-# DEPRECATED cauAWSAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'coIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauCoIP :: Lens.Lens' CoipAddressUsage (Lude.Maybe Lude.Text)
cauCoIP = Lens.lens (coIP :: CoipAddressUsage -> Lude.Maybe Lude.Text) (\s a -> s {coIP = a} :: CoipAddressUsage)
{-# DEPRECATED cauCoIP "Use generic-lens or generic-optics with 'coIP' instead." #-}

-- | The AWS service.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cauAWSService :: Lens.Lens' CoipAddressUsage (Lude.Maybe Lude.Text)
cauAWSService = Lens.lens (awsService :: CoipAddressUsage -> Lude.Maybe Lude.Text) (\s a -> s {awsService = a} :: CoipAddressUsage)
{-# DEPRECATED cauAWSService "Use generic-lens or generic-optics with 'awsService' instead." #-}

instance Lude.FromXML CoipAddressUsage where
  parseXML x =
    CoipAddressUsage'
      Lude.<$> (x Lude..@? "allocationId")
      Lude.<*> (x Lude..@? "awsAccountId")
      Lude.<*> (x Lude..@? "coIp")
      Lude.<*> (x Lude..@? "awsService")
