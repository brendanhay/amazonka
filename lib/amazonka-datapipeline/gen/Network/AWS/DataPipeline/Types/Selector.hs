{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Selector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.Selector
  ( Selector (..)
  -- * Smart constructor
  , mkSelector
  -- * Lenses
  , sFieldName
  , sOperator
  ) where

import qualified Network.AWS.DataPipeline.Types.Operator as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A comparision that is used to determine whether a query should return this object.
--
-- /See:/ 'mkSelector' smart constructor.
data Selector = Selector'
  { fieldName :: Core.Maybe Core.Text
    -- ^ The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
  , operator :: Core.Maybe Types.Operator
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Selector' value with any optional fields omitted.
mkSelector
    :: Selector
mkSelector
  = Selector'{fieldName = Core.Nothing, operator = Core.Nothing}

-- | The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFieldName :: Lens.Lens' Selector (Core.Maybe Core.Text)
sFieldName = Lens.field @"fieldName"
{-# INLINEABLE sFieldName #-}
{-# DEPRECATED fieldName "Use generic-lens or generic-optics with 'fieldName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOperator :: Lens.Lens' Selector (Core.Maybe Types.Operator)
sOperator = Lens.field @"operator"
{-# INLINEABLE sOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

instance Core.FromJSON Selector where
        toJSON Selector{..}
          = Core.object
              (Core.catMaybes
                 [("fieldName" Core..=) Core.<$> fieldName,
                  ("operator" Core..=) Core.<$> operator])
