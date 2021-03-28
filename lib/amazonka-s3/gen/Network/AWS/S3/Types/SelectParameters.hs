{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SelectParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.SelectParameters
  ( SelectParameters (..)
  -- * Smart constructor
  , mkSelectParameters
  -- * Lenses
  , spInputSerialization
  , spExpressionType
  , spExpression
  , spOutputSerialization
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Expression as Types
import qualified Network.AWS.S3.Types.ExpressionType as Types
import qualified Network.AWS.S3.Types.InputSerialization as Types
import qualified Network.AWS.S3.Types.OutputSerialization as Types

-- | Describes the parameters for Select job types.
--
-- /See:/ 'mkSelectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { inputSerialization :: Types.InputSerialization
    -- ^ Describes the serialization format of the object.
  , expressionType :: Types.ExpressionType
    -- ^ The type of the provided expression (for example, SQL).
  , expression :: Types.Expression
    -- ^ The expression that is used to query the object.
  , outputSerialization :: Types.OutputSerialization
    -- ^ Describes how the results of the Select job are serialized.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectParameters' value with any optional fields omitted.
mkSelectParameters
    :: Types.InputSerialization -- ^ 'inputSerialization'
    -> Types.ExpressionType -- ^ 'expressionType'
    -> Types.Expression -- ^ 'expression'
    -> Types.OutputSerialization -- ^ 'outputSerialization'
    -> SelectParameters
mkSelectParameters inputSerialization expressionType expression
  outputSerialization
  = SelectParameters'{inputSerialization, expressionType, expression,
                      outputSerialization}

-- | Describes the serialization format of the object.
--
-- /Note:/ Consider using 'inputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spInputSerialization :: Lens.Lens' SelectParameters Types.InputSerialization
spInputSerialization = Lens.field @"inputSerialization"
{-# INLINEABLE spInputSerialization #-}
{-# DEPRECATED inputSerialization "Use generic-lens or generic-optics with 'inputSerialization' instead"  #-}

-- | The type of the provided expression (for example, SQL).
--
-- /Note:/ Consider using 'expressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spExpressionType :: Lens.Lens' SelectParameters Types.ExpressionType
spExpressionType = Lens.field @"expressionType"
{-# INLINEABLE spExpressionType #-}
{-# DEPRECATED expressionType "Use generic-lens or generic-optics with 'expressionType' instead"  #-}

-- | The expression that is used to query the object.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spExpression :: Lens.Lens' SelectParameters Types.Expression
spExpression = Lens.field @"expression"
{-# INLINEABLE spExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | Describes how the results of the Select job are serialized.
--
-- /Note:/ Consider using 'outputSerialization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spOutputSerialization :: Lens.Lens' SelectParameters Types.OutputSerialization
spOutputSerialization = Lens.field @"outputSerialization"
{-# INLINEABLE spOutputSerialization #-}
{-# DEPRECATED outputSerialization "Use generic-lens or generic-optics with 'outputSerialization' instead"  #-}

instance Core.ToXML SelectParameters where
        toXML SelectParameters{..}
          = Core.toXMLElement "InputSerialization" inputSerialization Core.<>
              Core.toXMLElement "ExpressionType" expressionType
              Core.<> Core.toXMLElement "Expression" expression
              Core.<> Core.toXMLElement "OutputSerialization" outputSerialization
