{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputChannelMapping
  ( OutputChannelMapping (..),

    -- * Smart constructor
    mkOutputChannelMapping,

    -- * Lenses
    ocmInputChannels,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | OutputChannel mapping settings.
--
-- /See:/ 'mkOutputChannelMapping' smart constructor.
newtype OutputChannelMapping = OutputChannelMapping'
  { -- | List of input channels
    inputChannels :: Lude.Maybe [Lude.Int]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputChannelMapping' with the minimum fields required to make a request.
--
-- * 'inputChannels' - List of input channels
mkOutputChannelMapping ::
  OutputChannelMapping
mkOutputChannelMapping =
  OutputChannelMapping' {inputChannels = Lude.Nothing}

-- | List of input channels
--
-- /Note:/ Consider using 'inputChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocmInputChannels :: Lens.Lens' OutputChannelMapping (Lude.Maybe [Lude.Int])
ocmInputChannels = Lens.lens (inputChannels :: OutputChannelMapping -> Lude.Maybe [Lude.Int]) (\s a -> s {inputChannels = a} :: OutputChannelMapping)
{-# DEPRECATED ocmInputChannels "Use generic-lens or generic-optics with 'inputChannels' instead." #-}

instance Lude.FromJSON OutputChannelMapping where
  parseJSON =
    Lude.withObject
      "OutputChannelMapping"
      ( \x ->
          OutputChannelMapping'
            Lude.<$> (x Lude..:? "inputChannels" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON OutputChannelMapping where
  toJSON OutputChannelMapping' {..} =
    Lude.object
      (Lude.catMaybes [("inputChannels" Lude..=) Lude.<$> inputChannels])
