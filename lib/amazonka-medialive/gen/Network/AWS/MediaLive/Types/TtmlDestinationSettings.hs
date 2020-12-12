{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TtmlDestinationSettings
  ( TtmlDestinationSettings (..),

    -- * Smart constructor
    mkTtmlDestinationSettings,

    -- * Lenses
    tdsStyleControl,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
import qualified Network.AWS.Prelude as Lude

-- | Ttml Destination Settings
--
-- /See:/ 'mkTtmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { styleControl ::
      Lude.Maybe TtmlDestinationStyleControl
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TtmlDestinationSettings' with the minimum fields required to make a request.
--
-- * 'styleControl' - When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
mkTtmlDestinationSettings ::
  TtmlDestinationSettings
mkTtmlDestinationSettings =
  TtmlDestinationSettings' {styleControl = Lude.Nothing}

-- | When set to passthrough, passes through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
--
-- /Note:/ Consider using 'styleControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsStyleControl :: Lens.Lens' TtmlDestinationSettings (Lude.Maybe TtmlDestinationStyleControl)
tdsStyleControl = Lens.lens (styleControl :: TtmlDestinationSettings -> Lude.Maybe TtmlDestinationStyleControl) (\s a -> s {styleControl = a} :: TtmlDestinationSettings)
{-# DEPRECATED tdsStyleControl "Use generic-lens or generic-optics with 'styleControl' instead." #-}

instance Lude.FromJSON TtmlDestinationSettings where
  parseJSON =
    Lude.withObject
      "TtmlDestinationSettings"
      ( \x ->
          TtmlDestinationSettings' Lude.<$> (x Lude..:? "styleControl")
      )

instance Lude.ToJSON TtmlDestinationSettings where
  toJSON TtmlDestinationSettings' {..} =
    Lude.object
      (Lude.catMaybes [("styleControl" Lude..=) Lude.<$> styleControl])
