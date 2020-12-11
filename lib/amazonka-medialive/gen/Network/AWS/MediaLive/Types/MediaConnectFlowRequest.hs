-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlowRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaConnectFlowRequest
  ( MediaConnectFlowRequest (..),

    -- * Smart constructor
    mkMediaConnectFlowRequest,

    -- * Lenses
    mcfrFlowARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mkMediaConnectFlowRequest' smart constructor.
newtype MediaConnectFlowRequest = MediaConnectFlowRequest'
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

-- | Creates a value of 'MediaConnectFlowRequest' with the minimum fields required to make a request.
--
-- * 'flowARN' - The ARN of the MediaConnect Flow that you want to use as a source.
mkMediaConnectFlowRequest ::
  MediaConnectFlowRequest
mkMediaConnectFlowRequest =
  MediaConnectFlowRequest' {flowARN = Lude.Nothing}

-- | The ARN of the MediaConnect Flow that you want to use as a source.
--
-- /Note:/ Consider using 'flowARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcfrFlowARN :: Lens.Lens' MediaConnectFlowRequest (Lude.Maybe Lude.Text)
mcfrFlowARN = Lens.lens (flowARN :: MediaConnectFlowRequest -> Lude.Maybe Lude.Text) (\s a -> s {flowARN = a} :: MediaConnectFlowRequest)
{-# DEPRECATED mcfrFlowARN "Use generic-lens or generic-optics with 'flowARN' instead." #-}

instance Lude.ToJSON MediaConnectFlowRequest where
  toJSON MediaConnectFlowRequest' {..} =
    Lude.object
      (Lude.catMaybes [("flowArn" Lude..=) Lude.<$> flowARN])
