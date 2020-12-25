{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Delegate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Delegate
  ( Delegate (..),

    -- * Smart constructor
    mkDelegate,

    -- * Lenses
    dId,
    dType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.Id as Types
import qualified Network.AWS.WorkMail.Types.MemberType as Types

-- | The name of the attribute, which is one of the values defined in the UserAttribute enumeration.
--
-- /See:/ 'mkDelegate' smart constructor.
data Delegate = Delegate'
  { -- | The identifier for the user or group associated as the resource's delegate.
    id :: Types.Id,
    -- | The type of the delegate: user or group.
    type' :: Types.MemberType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Delegate' value with any optional fields omitted.
mkDelegate ::
  -- | 'id'
  Types.Id ->
  -- | 'type\''
  Types.MemberType ->
  Delegate
mkDelegate id type' = Delegate' {id, type'}

-- | The identifier for the user or group associated as the resource's delegate.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Delegate Types.Id
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the delegate: user or group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dType :: Lens.Lens' Delegate Types.MemberType
dType = Lens.field @"type'"
{-# DEPRECATED dType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Delegate where
  parseJSON =
    Core.withObject "Delegate" Core.$
      \x ->
        Delegate' Core.<$> (x Core..: "Id") Core.<*> (x Core..: "Type")
