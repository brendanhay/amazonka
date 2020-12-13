{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Selector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Selector
  ( Selector (..),

    -- * Smart constructor
    mkSelector,

    -- * Lenses
    sOperator,
    sFieldName,
  )
where

import Network.AWS.DataPipeline.Types.Operator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A comparision that is used to determine whether a query should return this object.
--
-- /See:/ 'mkSelector' smart constructor.
data Selector = Selector'
  { operator :: Lude.Maybe Operator,
    -- | The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
    fieldName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Selector' with the minimum fields required to make a request.
--
-- * 'operator' -
-- * 'fieldName' - The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
mkSelector ::
  Selector
mkSelector =
  Selector' {operator = Lude.Nothing, fieldName = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOperator :: Lens.Lens' Selector (Lude.Maybe Operator)
sOperator = Lens.lens (operator :: Selector -> Lude.Maybe Operator) (\s a -> s {operator = a} :: Selector)
{-# DEPRECATED sOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFieldName :: Lens.Lens' Selector (Lude.Maybe Lude.Text)
sFieldName = Lens.lens (fieldName :: Selector -> Lude.Maybe Lude.Text) (\s a -> s {fieldName = a} :: Selector)
{-# DEPRECATED sFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.ToJSON Selector where
  toJSON Selector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("operator" Lude..=) Lude.<$> operator,
            ("fieldName" Lude..=) Lude.<$> fieldName
          ]
      )
