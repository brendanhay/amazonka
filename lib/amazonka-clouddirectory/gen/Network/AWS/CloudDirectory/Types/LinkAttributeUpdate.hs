{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
  ( LinkAttributeUpdate (..),

    -- * Smart constructor
    mkLinkAttributeUpdate,

    -- * Lenses
    lauAttributeAction,
    lauAttributeKey,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.LinkAttributeAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Structure that contains attribute update information.
--
-- /See:/ 'mkLinkAttributeUpdate' smart constructor.
data LinkAttributeUpdate = LinkAttributeUpdate'
  { attributeAction ::
      Lude.Maybe LinkAttributeAction,
    attributeKey :: Lude.Maybe AttributeKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LinkAttributeUpdate' with the minimum fields required to make a request.
--
-- * 'attributeAction' - The action to perform as part of the attribute update.
-- * 'attributeKey' - The key of the attribute being updated.
mkLinkAttributeUpdate ::
  LinkAttributeUpdate
mkLinkAttributeUpdate =
  LinkAttributeUpdate'
    { attributeAction = Lude.Nothing,
      attributeKey = Lude.Nothing
    }

-- | The action to perform as part of the attribute update.
--
-- /Note:/ Consider using 'attributeAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauAttributeAction :: Lens.Lens' LinkAttributeUpdate (Lude.Maybe LinkAttributeAction)
lauAttributeAction = Lens.lens (attributeAction :: LinkAttributeUpdate -> Lude.Maybe LinkAttributeAction) (\s a -> s {attributeAction = a} :: LinkAttributeUpdate)
{-# DEPRECATED lauAttributeAction "Use generic-lens or generic-optics with 'attributeAction' instead." #-}

-- | The key of the attribute being updated.
--
-- /Note:/ Consider using 'attributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauAttributeKey :: Lens.Lens' LinkAttributeUpdate (Lude.Maybe AttributeKey)
lauAttributeKey = Lens.lens (attributeKey :: LinkAttributeUpdate -> Lude.Maybe AttributeKey) (\s a -> s {attributeKey = a} :: LinkAttributeUpdate)
{-# DEPRECATED lauAttributeKey "Use generic-lens or generic-optics with 'attributeKey' instead." #-}

instance Lude.ToJSON LinkAttributeUpdate where
  toJSON LinkAttributeUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeAction" Lude..=) Lude.<$> attributeAction,
            ("AttributeKey" Lude..=) Lude.<$> attributeKey
          ]
      )
