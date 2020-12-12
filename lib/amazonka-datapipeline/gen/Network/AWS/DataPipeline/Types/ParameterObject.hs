{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterObject
  ( ParameterObject (..),

    -- * Smart constructor
    mkParameterObject,

    -- * Lenses
    poId,
    poAttributes,
  )
where

import Network.AWS.DataPipeline.Types.ParameterAttribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a parameter object.
--
-- /See:/ 'mkParameterObject' smart constructor.
data ParameterObject = ParameterObject'
  { id :: Lude.Text,
    attributes :: [ParameterAttribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterObject' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes of the parameter object.
-- * 'id' - The ID of the parameter object.
mkParameterObject ::
  -- | 'id'
  Lude.Text ->
  ParameterObject
mkParameterObject pId_ =
  ParameterObject' {id = pId_, attributes = Lude.mempty}

-- | The ID of the parameter object.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poId :: Lens.Lens' ParameterObject Lude.Text
poId = Lens.lens (id :: ParameterObject -> Lude.Text) (\s a -> s {id = a} :: ParameterObject)
{-# DEPRECATED poId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The attributes of the parameter object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poAttributes :: Lens.Lens' ParameterObject [ParameterAttribute]
poAttributes = Lens.lens (attributes :: ParameterObject -> [ParameterAttribute]) (\s a -> s {attributes = a} :: ParameterObject)
{-# DEPRECATED poAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON ParameterObject where
  parseJSON =
    Lude.withObject
      "ParameterObject"
      ( \x ->
          ParameterObject'
            Lude.<$> (x Lude..: "id")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ParameterObject where
  toJSON ParameterObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("id" Lude..= id),
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )
