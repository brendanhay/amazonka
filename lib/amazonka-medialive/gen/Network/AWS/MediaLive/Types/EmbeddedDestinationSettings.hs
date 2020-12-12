{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
  ( EmbeddedDestinationSettings (..),

    -- * Smart constructor
    mkEmbeddedDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Embedded Destination Settings
--
-- /See:/ 'mkEmbeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmbeddedDestinationSettings' with the minimum fields required to make a request.
mkEmbeddedDestinationSettings ::
  EmbeddedDestinationSettings
mkEmbeddedDestinationSettings = EmbeddedDestinationSettings'

instance Lude.FromJSON EmbeddedDestinationSettings where
  parseJSON =
    Lude.withObject
      "EmbeddedDestinationSettings"
      (\x -> Lude.pure EmbeddedDestinationSettings')

instance Lude.ToJSON EmbeddedDestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
