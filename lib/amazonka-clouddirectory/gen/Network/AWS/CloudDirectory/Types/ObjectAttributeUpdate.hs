{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
  ( ObjectAttributeUpdate (..),

    -- * Smart constructor
    mkObjectAttributeUpdate,

    -- * Lenses
    oauObjectAttributeAction,
    oauObjectAttributeKey,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Structure that contains attribute update information.
--
-- /See:/ 'mkObjectAttributeUpdate' smart constructor.
data ObjectAttributeUpdate = ObjectAttributeUpdate'
  { -- | The action to perform as part of the attribute update.
    objectAttributeAction :: Lude.Maybe ObjectAttributeAction,
    -- | The key of the attribute being updated.
    objectAttributeKey :: Lude.Maybe AttributeKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectAttributeUpdate' with the minimum fields required to make a request.
--
-- * 'objectAttributeAction' - The action to perform as part of the attribute update.
-- * 'objectAttributeKey' - The key of the attribute being updated.
mkObjectAttributeUpdate ::
  ObjectAttributeUpdate
mkObjectAttributeUpdate =
  ObjectAttributeUpdate'
    { objectAttributeAction = Lude.Nothing,
      objectAttributeKey = Lude.Nothing
    }

-- | The action to perform as part of the attribute update.
--
-- /Note:/ Consider using 'objectAttributeAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oauObjectAttributeAction :: Lens.Lens' ObjectAttributeUpdate (Lude.Maybe ObjectAttributeAction)
oauObjectAttributeAction = Lens.lens (objectAttributeAction :: ObjectAttributeUpdate -> Lude.Maybe ObjectAttributeAction) (\s a -> s {objectAttributeAction = a} :: ObjectAttributeUpdate)
{-# DEPRECATED oauObjectAttributeAction "Use generic-lens or generic-optics with 'objectAttributeAction' instead." #-}

-- | The key of the attribute being updated.
--
-- /Note:/ Consider using 'objectAttributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oauObjectAttributeKey :: Lens.Lens' ObjectAttributeUpdate (Lude.Maybe AttributeKey)
oauObjectAttributeKey = Lens.lens (objectAttributeKey :: ObjectAttributeUpdate -> Lude.Maybe AttributeKey) (\s a -> s {objectAttributeKey = a} :: ObjectAttributeUpdate)
{-# DEPRECATED oauObjectAttributeKey "Use generic-lens or generic-optics with 'objectAttributeKey' instead." #-}

instance Lude.ToJSON ObjectAttributeUpdate where
  toJSON ObjectAttributeUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ObjectAttributeAction" Lude..=) Lude.<$> objectAttributeAction,
            ("ObjectAttributeKey" Lude..=) Lude.<$> objectAttributeKey
          ]
      )
