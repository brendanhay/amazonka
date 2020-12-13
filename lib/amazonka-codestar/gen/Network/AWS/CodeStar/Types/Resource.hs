{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a resource for a project.
--
-- /See:/ 'mkResource' smart constructor.
newtype Resource = Resource'
  { -- | The Amazon Resource Name (ARN) of the resource.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- * 'id' - The Amazon Resource Name (ARN) of the resource.
mkResource ::
  -- | 'id'
  Lude.Text ->
  Resource
mkResource pId_ = Resource' {id = pId_}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Resource Lude.Text
rId = Lens.lens (id :: Resource -> Lude.Text) (\s a -> s {id = a} :: Resource)
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Resource where
  parseJSON =
    Lude.withObject
      "Resource"
      (\x -> Resource' Lude.<$> (x Lude..: "id"))
