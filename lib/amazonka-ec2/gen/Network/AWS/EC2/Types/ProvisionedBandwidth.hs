{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProvisionedBandwidth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProvisionedBandwidth
  ( ProvisionedBandwidth (..),

    -- * Smart constructor
    mkProvisionedBandwidth,

    -- * Lenses
    pbStatus,
    pbRequested,
    pbProvisioned,
    pbRequestTime,
    pbProvisionTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /See:/ 'mkProvisionedBandwidth' smart constructor.
data ProvisionedBandwidth = ProvisionedBandwidth'
  { status ::
      Lude.Maybe Lude.Text,
    requested :: Lude.Maybe Lude.Text,
    provisioned :: Lude.Maybe Lude.Text,
    requestTime :: Lude.Maybe Lude.DateTime,
    provisionTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedBandwidth' with the minimum fields required to make a request.
--
-- * 'provisionTime' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
-- * 'provisioned' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
-- * 'requestTime' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
-- * 'requested' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
-- * 'status' - Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
mkProvisionedBandwidth ::
  ProvisionedBandwidth
mkProvisionedBandwidth =
  ProvisionedBandwidth'
    { status = Lude.Nothing,
      requested = Lude.Nothing,
      provisioned = Lude.Nothing,
      requestTime = Lude.Nothing,
      provisionTime = Lude.Nothing
    }

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbStatus :: Lens.Lens' ProvisionedBandwidth (Lude.Maybe Lude.Text)
pbStatus = Lens.lens (status :: ProvisionedBandwidth -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ProvisionedBandwidth)
{-# DEPRECATED pbStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'requested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbRequested :: Lens.Lens' ProvisionedBandwidth (Lude.Maybe Lude.Text)
pbRequested = Lens.lens (requested :: ProvisionedBandwidth -> Lude.Maybe Lude.Text) (\s a -> s {requested = a} :: ProvisionedBandwidth)
{-# DEPRECATED pbRequested "Use generic-lens or generic-optics with 'requested' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'provisioned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbProvisioned :: Lens.Lens' ProvisionedBandwidth (Lude.Maybe Lude.Text)
pbProvisioned = Lens.lens (provisioned :: ProvisionedBandwidth -> Lude.Maybe Lude.Text) (\s a -> s {provisioned = a} :: ProvisionedBandwidth)
{-# DEPRECATED pbProvisioned "Use generic-lens or generic-optics with 'provisioned' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'requestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbRequestTime :: Lens.Lens' ProvisionedBandwidth (Lude.Maybe Lude.DateTime)
pbRequestTime = Lens.lens (requestTime :: ProvisionedBandwidth -> Lude.Maybe Lude.DateTime) (\s a -> s {requestTime = a} :: ProvisionedBandwidth)
{-# DEPRECATED pbRequestTime "Use generic-lens or generic-optics with 'requestTime' instead." #-}

-- | Reserved. If you need to sustain traffic greater than the <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html documented limits> , contact us through the <https://console.aws.amazon.com/support/home? Support Center> .
--
-- /Note:/ Consider using 'provisionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbProvisionTime :: Lens.Lens' ProvisionedBandwidth (Lude.Maybe Lude.DateTime)
pbProvisionTime = Lens.lens (provisionTime :: ProvisionedBandwidth -> Lude.Maybe Lude.DateTime) (\s a -> s {provisionTime = a} :: ProvisionedBandwidth)
{-# DEPRECATED pbProvisionTime "Use generic-lens or generic-optics with 'provisionTime' instead." #-}

instance Lude.FromXML ProvisionedBandwidth where
  parseXML x =
    ProvisionedBandwidth'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "requested")
      Lude.<*> (x Lude..@? "provisioned")
      Lude.<*> (x Lude..@? "requestTime")
      Lude.<*> (x Lude..@? "provisionTime")
