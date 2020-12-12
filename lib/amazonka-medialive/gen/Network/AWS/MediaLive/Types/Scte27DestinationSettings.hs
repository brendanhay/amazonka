{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte27DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte27DestinationSettings
  ( Scte27DestinationSettings (..),

    -- * Smart constructor
    mkScte27DestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Scte27 Destination Settings
--
-- /See:/ 'mkScte27DestinationSettings' smart constructor.
data Scte27DestinationSettings = Scte27DestinationSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte27DestinationSettings' with the minimum fields required to make a request.
mkScte27DestinationSettings ::
  Scte27DestinationSettings
mkScte27DestinationSettings = Scte27DestinationSettings'

instance Lude.FromJSON Scte27DestinationSettings where
  parseJSON =
    Lude.withObject
      "Scte27DestinationSettings"
      (\x -> Lude.pure Scte27DestinationSettings')

instance Lude.ToJSON Scte27DestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
