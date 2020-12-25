{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Dimension
  ( Dimension (..),

    -- * Smart constructor
    mkDimension,

    -- * Lenses
    dName,
    dValue,
  )
where

import qualified Network.AWS.CloudWatch.Types.Name as Types
import qualified Network.AWS.CloudWatch.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A dimension is a name/value pair that is part of the identity of a metric. You can assign up to 10 dimensions to a metric. Because dimensions are part of the unique identifier for a metric, whenever you add a unique name/value pair to one of your metrics, you are creating a new variation of that metric.
--
-- /See:/ 'mkDimension' smart constructor.
data Dimension = Dimension'
  { -- | The name of the dimension. Dimension names cannot contain blank spaces or non-ASCII characters.
    name :: Types.Name,
    -- | The value of the dimension. Dimension values cannot contain blank spaces or non-ASCII characters.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Dimension' value with any optional fields omitted.
mkDimension ::
  -- | 'name'
  Types.Name ->
  -- | 'value'
  Types.Value ->
  Dimension
mkDimension name value = Dimension' {name, value}

-- | The name of the dimension. Dimension names cannot contain blank spaces or non-ASCII characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Dimension Types.Name
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the dimension. Dimension values cannot contain blank spaces or non-ASCII characters.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dValue :: Lens.Lens' Dimension Types.Value
dValue = Lens.field @"value"
{-# DEPRECATED dValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML Dimension where
  parseXML x =
    Dimension'
      Core.<$> (x Core..@ "Name") Core.<*> (x Core..@ "Value")
