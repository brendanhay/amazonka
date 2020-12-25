{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | Rec709 Settings
--
-- /See:/ 'mkRec709Settings' smart constructor.
data Rec709Settings = Rec709Settings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rec709Settings' value with any optional fields omitted.
mkRec709Settings ::
  Rec709Settings
mkRec709Settings = Rec709Settings'

instance Core.FromJSON Rec709Settings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON Rec709Settings where
  parseJSON =
    Core.withObject "Rec709Settings" Core.$
      \x -> Core.pure Rec709Settings'
