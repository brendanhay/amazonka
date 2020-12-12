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

import Network.AWS.DirectoryService.Types.TargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifier that contains details about the directory consumer account with whom the directory is being unshared.
--
-- /See:/ 'mkUnshareTarget' smart constructor.
data UnshareTarget = UnshareTarget'
  { id :: Lude.Text,
    type' :: TargetType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnshareTarget' with the minimum fields required to make a request.
--
-- * 'id' - Identifier of the directory consumer account.
-- * 'type'' - Type of identifier to be used in the /Id/ field.
mkUnshareTarget ::
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  TargetType ->
  UnshareTarget
mkUnshareTarget pId_ pType_ =
  UnshareTarget' {id = pId_, type' = pType_}

-- | Identifier of the directory consumer account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utId :: Lens.Lens' UnshareTarget Lude.Text
utId = Lens.lens (id :: UnshareTarget -> Lude.Text) (\s a -> s {id = a} :: UnshareTarget)
{-# DEPRECATED utId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Type of identifier to be used in the /Id/ field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utType :: Lens.Lens' UnshareTarget TargetType
utType = Lens.lens (type' :: UnshareTarget -> TargetType) (\s a -> s {type' = a} :: UnshareTarget)
{-# DEPRECATED utType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON UnshareTarget where
  toJSON UnshareTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Id" Lude..= id), Lude.Just ("Type" Lude..= type')]
      )
