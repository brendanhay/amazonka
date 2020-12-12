{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexGroupSettings
  ( MultiplexGroupSettings (..),

    -- * Smart constructor
    mkMultiplexGroupSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Multiplex Group Settings
--
-- /See:/ 'mkMultiplexGroupSettings' smart constructor.
data MultiplexGroupSettings = MultiplexGroupSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexGroupSettings' with the minimum fields required to make a request.
mkMultiplexGroupSettings ::
  MultiplexGroupSettings
mkMultiplexGroupSettings = MultiplexGroupSettings'

instance Lude.FromJSON MultiplexGroupSettings where
  parseJSON =
    Lude.withObject
      "MultiplexGroupSettings"
      (\x -> Lude.pure MultiplexGroupSettings')

instance Lude.ToJSON MultiplexGroupSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
