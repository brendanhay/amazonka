{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SharePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.SharePrincipal
  ( SharePrincipal (..),

    -- * Smart constructor
    mkSharePrincipal,

    -- * Lenses
    spId,
    spType,
    spRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.Id as Types
import qualified Network.AWS.WorkDocs.Types.PrincipalType as Types
import qualified Network.AWS.WorkDocs.Types.RoleType as Types

-- | Describes the recipient type and ID, if available.
--
-- /See:/ 'mkSharePrincipal' smart constructor.
data SharePrincipal = SharePrincipal'
  { -- | The ID of the recipient.
    id :: Types.Id,
    -- | The type of the recipient.
    type' :: Types.PrincipalType,
    -- | The role of the recipient.
    role' :: Types.RoleType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SharePrincipal' value with any optional fields omitted.
mkSharePrincipal ::
  -- | 'id'
  Types.Id ->
  -- | 'type\''
  Types.PrincipalType ->
  -- | 'role\''
  Types.RoleType ->
  SharePrincipal
mkSharePrincipal id type' role' = SharePrincipal' {id, type', role'}

-- | The ID of the recipient.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spId :: Lens.Lens' SharePrincipal Types.Id
spId = Lens.field @"id"
{-# DEPRECATED spId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the recipient.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spType :: Lens.Lens' SharePrincipal Types.PrincipalType
spType = Lens.field @"type'"
{-# DEPRECATED spType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The role of the recipient.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spRole :: Lens.Lens' SharePrincipal Types.RoleType
spRole = Lens.field @"role'"
{-# DEPRECATED spRole "Use generic-lens or generic-optics with 'role'' instead." #-}

instance Core.FromJSON SharePrincipal where
  toJSON SharePrincipal {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            Core.Just ("Type" Core..= type'),
            Core.Just ("Role" Core..= role')
          ]
      )
