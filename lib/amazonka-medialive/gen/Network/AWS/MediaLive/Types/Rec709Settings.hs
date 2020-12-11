-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Rec709Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec709Settings
  ( Rec709Settings (..),

    -- * Smart constructor
    mkRec709Settings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Rec709 Settings
--
-- /See:/ 'mkRec709Settings' smart constructor.
data Rec709Settings = Rec709Settings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rec709Settings' with the minimum fields required to make a request.
mkRec709Settings ::
  Rec709Settings
mkRec709Settings = Rec709Settings'

instance Lude.FromJSON Rec709Settings where
  parseJSON =
    Lude.withObject
      "Rec709Settings"
      (\x -> Lude.pure Rec709Settings')

instance Lude.ToJSON Rec709Settings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
