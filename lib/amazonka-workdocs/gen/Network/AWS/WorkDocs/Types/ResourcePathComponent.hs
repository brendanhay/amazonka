-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourcePathComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourcePathComponent
  ( ResourcePathComponent (..),

    -- * Smart constructor
    mkResourcePathComponent,

    -- * Lenses
    rpcName,
    rpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the resource path.
--
-- /See:/ 'mkResourcePathComponent' smart constructor.
data ResourcePathComponent = ResourcePathComponent'
  { name ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourcePathComponent' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the resource path.
-- * 'name' - The name of the resource path.
mkResourcePathComponent ::
  ResourcePathComponent
mkResourcePathComponent =
  ResourcePathComponent' {name = Lude.Nothing, id = Lude.Nothing}

-- | The name of the resource path.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpcName :: Lens.Lens' ResourcePathComponent (Lude.Maybe Lude.Text)
rpcName = Lens.lens (name :: ResourcePathComponent -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourcePathComponent)
{-# DEPRECATED rpcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the resource path.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpcId :: Lens.Lens' ResourcePathComponent (Lude.Maybe Lude.Text)
rpcId = Lens.lens (id :: ResourcePathComponent -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ResourcePathComponent)
{-# DEPRECATED rpcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ResourcePathComponent where
  parseJSON =
    Lude.withObject
      "ResourcePathComponent"
      ( \x ->
          ResourcePathComponent'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Id")
      )
