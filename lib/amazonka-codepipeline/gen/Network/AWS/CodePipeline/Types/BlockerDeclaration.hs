-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.BlockerDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.BlockerDeclaration
  ( BlockerDeclaration (..),

    -- * Smart constructor
    mkBlockerDeclaration,

    -- * Lenses
    bdName,
    bdType,
  )
where

import Network.AWS.CodePipeline.Types.BlockerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Reserved for future use.
--
-- /See:/ 'mkBlockerDeclaration' smart constructor.
data BlockerDeclaration = BlockerDeclaration'
  { name :: Lude.Text,
    type' :: BlockerType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockerDeclaration' with the minimum fields required to make a request.
--
-- * 'name' - Reserved for future use.
-- * 'type'' - Reserved for future use.
mkBlockerDeclaration ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  BlockerType ->
  BlockerDeclaration
mkBlockerDeclaration pName_ pType_ =
  BlockerDeclaration' {name = pName_, type' = pType_}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdName :: Lens.Lens' BlockerDeclaration Lude.Text
bdName = Lens.lens (name :: BlockerDeclaration -> Lude.Text) (\s a -> s {name = a} :: BlockerDeclaration)
{-# DEPRECATED bdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdType :: Lens.Lens' BlockerDeclaration BlockerType
bdType = Lens.lens (type' :: BlockerDeclaration -> BlockerType) (\s a -> s {type' = a} :: BlockerDeclaration)
{-# DEPRECATED bdType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON BlockerDeclaration where
  parseJSON =
    Lude.withObject
      "BlockerDeclaration"
      ( \x ->
          BlockerDeclaration'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON BlockerDeclaration where
  toJSON BlockerDeclaration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("type" Lude..= type')
          ]
      )
