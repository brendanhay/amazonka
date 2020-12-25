{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Rec601Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec601Settings
  ( Rec601Settings (..),

    -- * Smart constructor
    mkRec601Settings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Rec601 Settings
--
-- /See:/ 'mkRec601Settings' smart constructor.
data Rec601Settings = Rec601Settings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Rec601Settings' value with any optional fields omitted.
mkRec601Settings ::
  Rec601Settings
mkRec601Settings = Rec601Settings'

instance Core.FromJSON Rec601Settings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON Rec601Settings where
  parseJSON =
    Core.withObject "Rec601Settings" Core.$
      \x -> Core.pure Rec601Settings'
