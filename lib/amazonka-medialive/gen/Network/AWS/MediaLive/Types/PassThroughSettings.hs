{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PassThroughSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PassThroughSettings
  ( PassThroughSettings (..),

    -- * Smart constructor
    mkPassThroughSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Pass Through Settings
--
-- /See:/ 'mkPassThroughSettings' smart constructor.
data PassThroughSettings = PassThroughSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PassThroughSettings' with the minimum fields required to make a request.
mkPassThroughSettings ::
  PassThroughSettings
mkPassThroughSettings = PassThroughSettings'

instance Lude.FromJSON PassThroughSettings where
  parseJSON =
    Lude.withObject
      "PassThroughSettings"
      (\x -> Lude.pure PassThroughSettings')

instance Lude.ToJSON PassThroughSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
