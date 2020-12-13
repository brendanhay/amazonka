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

import Network.AWS.IoT.Types.ActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of authorization information.
--
-- /See:/ 'mkAuthInfo' smart constructor.
data AuthInfo = AuthInfo'
  { -- | The resources for which the principal is being authorized to perform the specified action.
    resources :: [Lude.Text],
    -- | The type of action for which the principal is being authorized.
    actionType :: Lude.Maybe ActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthInfo' with the minimum fields required to make a request.
--
-- * 'resources' - The resources for which the principal is being authorized to perform the specified action.
-- * 'actionType' - The type of action for which the principal is being authorized.
mkAuthInfo ::
  AuthInfo
mkAuthInfo =
  AuthInfo' {resources = Lude.mempty, actionType = Lude.Nothing}

-- | The resources for which the principal is being authorized to perform the specified action.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiResources :: Lens.Lens' AuthInfo [Lude.Text]
aiResources = Lens.lens (resources :: AuthInfo -> [Lude.Text]) (\s a -> s {resources = a} :: AuthInfo)
{-# DEPRECATED aiResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The type of action for which the principal is being authorized.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiActionType :: Lens.Lens' AuthInfo (Lude.Maybe ActionType)
aiActionType = Lens.lens (actionType :: AuthInfo -> Lude.Maybe ActionType) (\s a -> s {actionType = a} :: AuthInfo)
{-# DEPRECATED aiActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Lude.FromJSON AuthInfo where
  parseJSON =
    Lude.withObject
      "AuthInfo"
      ( \x ->
          AuthInfo'
            Lude.<$> (x Lude..:? "resources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "actionType")
      )

instance Lude.ToJSON AuthInfo where
  toJSON AuthInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("resources" Lude..= resources),
            ("actionType" Lude..=) Lude.<$> actionType
          ]
      )
