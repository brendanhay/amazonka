{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareTarget
  ( ShareTarget (..),

    -- * Smart constructor
    mkShareTarget,

    -- * Lenses
    stId,
    stType,
  )
where

import Network.AWS.DirectoryService.Types.TargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifier that contains details about the directory consumer account.
--
-- /See:/ 'mkShareTarget' smart constructor.
data ShareTarget = ShareTarget'
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

-- | Creates a value of 'ShareTarget' with the minimum fields required to make a request.
--
-- * 'id' - Identifier of the directory consumer account.
-- * 'type'' - Type of identifier to be used in the @Id@ field.
mkShareTarget ::
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  TargetType ->
  ShareTarget
mkShareTarget pId_ pType_ = ShareTarget' {id = pId_, type' = pType_}

-- | Identifier of the directory consumer account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stId :: Lens.Lens' ShareTarget Lude.Text
stId = Lens.lens (id :: ShareTarget -> Lude.Text) (\s a -> s {id = a} :: ShareTarget)
{-# DEPRECATED stId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Type of identifier to be used in the @Id@ field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stType :: Lens.Lens' ShareTarget TargetType
stType = Lens.lens (type' :: ShareTarget -> TargetType) (\s a -> s {type' = a} :: ShareTarget)
{-# DEPRECATED stType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON ShareTarget where
  toJSON ShareTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Id" Lude..= id), Lude.Just ("Type" Lude..= type')]
      )
