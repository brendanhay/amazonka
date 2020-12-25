{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.UnshareTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.UnshareTarget
  ( UnshareTarget (..),

    -- * Smart constructor
    mkUnshareTarget,

    -- * Lenses
    utId,
    utType,
  )
where

import qualified Network.AWS.DirectoryService.Types.TargetId as Types
import qualified Network.AWS.DirectoryService.Types.TargetType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifier that contains details about the directory consumer account with whom the directory is being unshared.
--
-- /See:/ 'mkUnshareTarget' smart constructor.
data UnshareTarget = UnshareTarget'
  { -- | Identifier of the directory consumer account.
    id :: Types.TargetId,
    -- | Type of identifier to be used in the /Id/ field.
    type' :: Types.TargetType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnshareTarget' value with any optional fields omitted.
mkUnshareTarget ::
  -- | 'id'
  Types.TargetId ->
  -- | 'type\''
  Types.TargetType ->
  UnshareTarget
mkUnshareTarget id type' = UnshareTarget' {id, type'}

-- | Identifier of the directory consumer account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utId :: Lens.Lens' UnshareTarget Types.TargetId
utId = Lens.field @"id"
{-# DEPRECATED utId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Type of identifier to be used in the /Id/ field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utType :: Lens.Lens' UnshareTarget Types.TargetType
utType = Lens.field @"type'"
{-# DEPRECATED utType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON UnshareTarget where
  toJSON UnshareTarget {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Id" Core..= id), Core.Just ("Type" Core..= type')]
      )
