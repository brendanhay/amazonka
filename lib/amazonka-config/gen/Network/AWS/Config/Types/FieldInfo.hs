{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FieldInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FieldInfo
  ( FieldInfo (..),

    -- * Smart constructor
    mkFieldInfo,

    -- * Lenses
    fiName,
  )
where

import qualified Network.AWS.Config.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the fields such as name of the field.
--
-- /See:/ 'mkFieldInfo' smart constructor.
newtype FieldInfo = FieldInfo'
  { -- | Name of the field.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FieldInfo' value with any optional fields omitted.
mkFieldInfo ::
  FieldInfo
mkFieldInfo = FieldInfo' {name = Core.Nothing}

-- | Name of the field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiName :: Lens.Lens' FieldInfo (Core.Maybe Types.Name)
fiName = Lens.field @"name"
{-# DEPRECATED fiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON FieldInfo where
  parseJSON =
    Core.withObject "FieldInfo" Core.$
      \x -> FieldInfo' Core.<$> (x Core..:? "Name")
