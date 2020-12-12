{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RootCause
  ( RootCause (..),

    -- * Smart constructor
    mkRootCause,

    -- * Lenses
    rcService,
    rcUsageType,
    rcLinkedAccount,
    rcRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The combination of AWS service, linked account, Region, and usage type where a cost anomaly is observed.
--
-- /See:/ 'mkRootCause' smart constructor.
data RootCause = RootCause'
  { service :: Lude.Maybe Lude.Text,
    usageType :: Lude.Maybe Lude.Text,
    linkedAccount :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RootCause' with the minimum fields required to make a request.
--
-- * 'linkedAccount' - The linked account value associated with the cost anomaly.
-- * 'region' - The AWS Region associated with the cost anomaly.
-- * 'service' - The AWS service name associated with the cost anomaly.
-- * 'usageType' - The @UsageType@ value associated with the cost anomaly.
mkRootCause ::
  RootCause
mkRootCause =
  RootCause'
    { service = Lude.Nothing,
      usageType = Lude.Nothing,
      linkedAccount = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The AWS service name associated with the cost anomaly.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcService :: Lens.Lens' RootCause (Lude.Maybe Lude.Text)
rcService = Lens.lens (service :: RootCause -> Lude.Maybe Lude.Text) (\s a -> s {service = a} :: RootCause)
{-# DEPRECATED rcService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The @UsageType@ value associated with the cost anomaly.
--
-- /Note:/ Consider using 'usageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcUsageType :: Lens.Lens' RootCause (Lude.Maybe Lude.Text)
rcUsageType = Lens.lens (usageType :: RootCause -> Lude.Maybe Lude.Text) (\s a -> s {usageType = a} :: RootCause)
{-# DEPRECATED rcUsageType "Use generic-lens or generic-optics with 'usageType' instead." #-}

-- | The linked account value associated with the cost anomaly.
--
-- /Note:/ Consider using 'linkedAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLinkedAccount :: Lens.Lens' RootCause (Lude.Maybe Lude.Text)
rcLinkedAccount = Lens.lens (linkedAccount :: RootCause -> Lude.Maybe Lude.Text) (\s a -> s {linkedAccount = a} :: RootCause)
{-# DEPRECATED rcLinkedAccount "Use generic-lens or generic-optics with 'linkedAccount' instead." #-}

-- | The AWS Region associated with the cost anomaly.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRegion :: Lens.Lens' RootCause (Lude.Maybe Lude.Text)
rcRegion = Lens.lens (region :: RootCause -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: RootCause)
{-# DEPRECATED rcRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON RootCause where
  parseJSON =
    Lude.withObject
      "RootCause"
      ( \x ->
          RootCause'
            Lude.<$> (x Lude..:? "Service")
            Lude.<*> (x Lude..:? "UsageType")
            Lude.<*> (x Lude..:? "LinkedAccount")
            Lude.<*> (x Lude..:? "Region")
      )
