{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthInfo
  ( AuthInfo (..),

    -- * Smart constructor
    mkAuthInfo,

    -- * Lenses
    aiResources,
    aiActionType,
  )
where

import qualified Network.AWS.IoT.Types.ActionType as Types
import qualified Network.AWS.IoT.Types.Resource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of authorization information.
--
-- /See:/ 'mkAuthInfo' smart constructor.
data AuthInfo = AuthInfo'
  { -- | The resources for which the principal is being authorized to perform the specified action.
    resources :: [Types.Resource],
    -- | The type of action for which the principal is being authorized.
    actionType :: Core.Maybe Types.ActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthInfo' value with any optional fields omitted.
mkAuthInfo ::
  AuthInfo
mkAuthInfo =
  AuthInfo' {resources = Core.mempty, actionType = Core.Nothing}

-- | The resources for which the principal is being authorized to perform the specified action.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiResources :: Lens.Lens' AuthInfo [Types.Resource]
aiResources = Lens.field @"resources"
{-# DEPRECATED aiResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The type of action for which the principal is being authorized.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiActionType :: Lens.Lens' AuthInfo (Core.Maybe Types.ActionType)
aiActionType = Lens.field @"actionType"
{-# DEPRECATED aiActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Core.FromJSON AuthInfo where
  toJSON AuthInfo {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resources" Core..= resources),
            ("actionType" Core..=) Core.<$> actionType
          ]
      )

instance Core.FromJSON AuthInfo where
  parseJSON =
    Core.withObject "AuthInfo" Core.$
      \x ->
        AuthInfo'
          Core.<$> (x Core..:? "resources" Core..!= Core.mempty)
          Core.<*> (x Core..:? "actionType")
