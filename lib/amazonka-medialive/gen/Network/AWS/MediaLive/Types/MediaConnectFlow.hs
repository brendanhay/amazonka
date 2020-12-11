-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaConnectFlow
  ( MediaConnectFlow (..),

    -- * Smart constructor
    mkMediaConnectFlow,

    -- * Lenses
    mcfFlowARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mkMediaConnectFlow' smart constructor.
newtype MediaConnectFlow = MediaConnectFlow'
  { flowARN ::
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

-- | Creates a value of 'MediaConnectFlow' with the minimum fields required to make a request.
--
-- * 'flowARN' - The unique ARN of the MediaConnect Flow being used as a source.
mkMediaConnectFlow ::
  MediaConnectFlow
mkMediaConnectFlow = MediaConnectFlow' {flowARN = Lude.Nothing}

-- | The unique ARN of the MediaConnect Flow being used as a source.
--
-- /Note:/ Consider using 'flowARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcfFlowARN :: Lens.Lens' MediaConnectFlow (Lude.Maybe Lude.Text)
mcfFlowARN = Lens.lens (flowARN :: MediaConnectFlow -> Lude.Maybe Lude.Text) (\s a -> s {flowARN = a} :: MediaConnectFlow)
{-# DEPRECATED mcfFlowARN "Use generic-lens or generic-optics with 'flowARN' instead." #-}

instance Lude.FromJSON MediaConnectFlow where
  parseJSON =
    Lude.withObject
      "MediaConnectFlow"
      (\x -> MediaConnectFlow' Lude.<$> (x Lude..:? "flowArn"))
