{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOnRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOnRequest
  ( AddOnRequest (..),

    -- * Smart constructor
    mkAddOnRequest,

    -- * Lenses
    aorAutoSnapshotAddOnRequest,
    aorAddOnType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AddOnType
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import qualified Network.AWS.Prelude as Lude

-- | Describes a request to enable, modify, or disable an add-on for an Amazon Lightsail resource.
--
-- /See:/ 'mkAddOnRequest' smart constructor.
data AddOnRequest = AddOnRequest'
  { autoSnapshotAddOnRequest ::
      Lude.Maybe AutoSnapshotAddOnRequest,
    addOnType :: AddOnType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddOnRequest' with the minimum fields required to make a request.
--
-- * 'addOnType' - The add-on type.
-- * 'autoSnapshotAddOnRequest' - An object that represents additional parameters when enabling or modifying the automatic snapshot add-on.
mkAddOnRequest ::
  -- | 'addOnType'
  AddOnType ->
  AddOnRequest
mkAddOnRequest pAddOnType_ =
  AddOnRequest'
    { autoSnapshotAddOnRequest = Lude.Nothing,
      addOnType = pAddOnType_
    }

-- | An object that represents additional parameters when enabling or modifying the automatic snapshot add-on.
--
-- /Note:/ Consider using 'autoSnapshotAddOnRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aorAutoSnapshotAddOnRequest :: Lens.Lens' AddOnRequest (Lude.Maybe AutoSnapshotAddOnRequest)
aorAutoSnapshotAddOnRequest = Lens.lens (autoSnapshotAddOnRequest :: AddOnRequest -> Lude.Maybe AutoSnapshotAddOnRequest) (\s a -> s {autoSnapshotAddOnRequest = a} :: AddOnRequest)
{-# DEPRECATED aorAutoSnapshotAddOnRequest "Use generic-lens or generic-optics with 'autoSnapshotAddOnRequest' instead." #-}

-- | The add-on type.
--
-- /Note:/ Consider using 'addOnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aorAddOnType :: Lens.Lens' AddOnRequest AddOnType
aorAddOnType = Lens.lens (addOnType :: AddOnRequest -> AddOnType) (\s a -> s {addOnType = a} :: AddOnRequest)
{-# DEPRECATED aorAddOnType "Use generic-lens or generic-optics with 'addOnType' instead." #-}

instance Lude.ToJSON AddOnRequest where
  toJSON AddOnRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("autoSnapshotAddOnRequest" Lude..=)
              Lude.<$> autoSnapshotAddOnRequest,
            Lude.Just ("addOnType" Lude..= addOnType)
          ]
      )
