-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RawSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RawSettings
  ( RawSettings (..),

    -- * Smart constructor
    mkRawSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Raw Settings
--
-- /See:/ 'mkRawSettings' smart constructor.
data RawSettings = RawSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RawSettings' with the minimum fields required to make a request.
mkRawSettings ::
  RawSettings
mkRawSettings = RawSettings'

instance Lude.FromJSON RawSettings where
  parseJSON =
    Lude.withObject "RawSettings" (\x -> Lude.pure RawSettings')

instance Lude.ToJSON RawSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
