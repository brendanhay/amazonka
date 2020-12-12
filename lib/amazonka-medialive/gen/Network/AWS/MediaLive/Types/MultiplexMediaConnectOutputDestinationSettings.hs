{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
  ( MultiplexMediaConnectOutputDestinationSettings (..),

    -- * Smart constructor
    mkMultiplexMediaConnectOutputDestinationSettings,

    -- * Lenses
    mmcodsEntitlementARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Multiplex MediaConnect output destination settings.
--
-- /See:/ 'mkMultiplexMediaConnectOutputDestinationSettings' smart constructor.
newtype MultiplexMediaConnectOutputDestinationSettings = MultiplexMediaConnectOutputDestinationSettings'
  { entitlementARN ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'MultiplexMediaConnectOutputDestinationSettings' with the minimum fields required to make a request.
--
-- * 'entitlementARN' - The MediaConnect entitlement ARN available as a Flow source.
mkMultiplexMediaConnectOutputDestinationSettings ::
  MultiplexMediaConnectOutputDestinationSettings
mkMultiplexMediaConnectOutputDestinationSettings =
  MultiplexMediaConnectOutputDestinationSettings'
    { entitlementARN =
        Lude.Nothing
    }

-- | The MediaConnect entitlement ARN available as a Flow source.
--
-- /Note:/ Consider using 'entitlementARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mmcodsEntitlementARN :: Lens.Lens' MultiplexMediaConnectOutputDestinationSettings (Lude.Maybe Lude.Text)
mmcodsEntitlementARN = Lens.lens (entitlementARN :: MultiplexMediaConnectOutputDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {entitlementARN = a} :: MultiplexMediaConnectOutputDestinationSettings)
{-# DEPRECATED mmcodsEntitlementARN "Use generic-lens or generic-optics with 'entitlementARN' instead." #-}

instance
  Lude.FromJSON
    MultiplexMediaConnectOutputDestinationSettings
  where
  parseJSON =
    Lude.withObject
      "MultiplexMediaConnectOutputDestinationSettings"
      ( \x ->
          MultiplexMediaConnectOutputDestinationSettings'
            Lude.<$> (x Lude..:? "entitlementArn")
      )
