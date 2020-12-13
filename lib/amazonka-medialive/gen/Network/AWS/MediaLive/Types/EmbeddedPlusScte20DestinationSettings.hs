{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
  ( EmbeddedPlusScte20DestinationSettings (..),

    -- * Smart constructor
    mkEmbeddedPlusScte20DestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Embedded Plus Scte20 Destination Settings
--
-- /See:/ 'mkEmbeddedPlusScte20DestinationSettings' smart constructor.
data EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmbeddedPlusScte20DestinationSettings' with the minimum fields required to make a request.
mkEmbeddedPlusScte20DestinationSettings ::
  EmbeddedPlusScte20DestinationSettings
mkEmbeddedPlusScte20DestinationSettings =
  EmbeddedPlusScte20DestinationSettings'

instance Lude.FromJSON EmbeddedPlusScte20DestinationSettings where
  parseJSON =
    Lude.withObject
      "EmbeddedPlusScte20DestinationSettings"
      (\x -> Lude.pure EmbeddedPlusScte20DestinationSettings')

instance Lude.ToJSON EmbeddedPlusScte20DestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
