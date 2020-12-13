{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
  ( SingleMasterConfiguration (..),

    -- * Smart constructor
    mkSingleMasterConfiguration,

    -- * Lenses
    smcMessageTtlSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains the configuration for the @SINGLE_MASTER@ channel type.
--
-- /See:/ 'mkSingleMasterConfiguration' smart constructor.
newtype SingleMasterConfiguration = SingleMasterConfiguration'
  { -- | The period of time a signaling channel retains underlivered messages before they are discarded.
    messageTtlSeconds :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SingleMasterConfiguration' with the minimum fields required to make a request.
--
-- * 'messageTtlSeconds' - The period of time a signaling channel retains underlivered messages before they are discarded.
mkSingleMasterConfiguration ::
  SingleMasterConfiguration
mkSingleMasterConfiguration =
  SingleMasterConfiguration' {messageTtlSeconds = Lude.Nothing}

-- | The period of time a signaling channel retains underlivered messages before they are discarded.
--
-- /Note:/ Consider using 'messageTtlSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smcMessageTtlSeconds :: Lens.Lens' SingleMasterConfiguration (Lude.Maybe Lude.Natural)
smcMessageTtlSeconds = Lens.lens (messageTtlSeconds :: SingleMasterConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {messageTtlSeconds = a} :: SingleMasterConfiguration)
{-# DEPRECATED smcMessageTtlSeconds "Use generic-lens or generic-optics with 'messageTtlSeconds' instead." #-}

instance Lude.FromJSON SingleMasterConfiguration where
  parseJSON =
    Lude.withObject
      "SingleMasterConfiguration"
      ( \x ->
          SingleMasterConfiguration'
            Lude.<$> (x Lude..:? "MessageTtlSeconds")
      )

instance Lude.ToJSON SingleMasterConfiguration where
  toJSON SingleMasterConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("MessageTtlSeconds" Lude..=) Lude.<$> messageTtlSeconds]
      )
