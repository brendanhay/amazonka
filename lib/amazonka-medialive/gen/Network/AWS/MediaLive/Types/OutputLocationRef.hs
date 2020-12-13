{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputLocationRef
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputLocationRef
  ( OutputLocationRef (..),

    -- * Smart constructor
    mkOutputLocationRef,

    -- * Lenses
    olrDestinationRefId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Reference to an OutputDestination ID defined in the channel
--
-- /See:/ 'mkOutputLocationRef' smart constructor.
newtype OutputLocationRef = OutputLocationRef'
  { destinationRefId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputLocationRef' with the minimum fields required to make a request.
--
-- * 'destinationRefId' -
mkOutputLocationRef ::
  OutputLocationRef
mkOutputLocationRef =
  OutputLocationRef' {destinationRefId = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'destinationRefId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrDestinationRefId :: Lens.Lens' OutputLocationRef (Lude.Maybe Lude.Text)
olrDestinationRefId = Lens.lens (destinationRefId :: OutputLocationRef -> Lude.Maybe Lude.Text) (\s a -> s {destinationRefId = a} :: OutputLocationRef)
{-# DEPRECATED olrDestinationRefId "Use generic-lens or generic-optics with 'destinationRefId' instead." #-}

instance Lude.FromJSON OutputLocationRef where
  parseJSON =
    Lude.withObject
      "OutputLocationRef"
      ( \x ->
          OutputLocationRef' Lude.<$> (x Lude..:? "destinationRefId")
      )

instance Lude.ToJSON OutputLocationRef where
  toJSON OutputLocationRef' {..} =
    Lude.object
      ( Lude.catMaybes
          [("destinationRefId" Lude..=) Lude.<$> destinationRefId]
      )
