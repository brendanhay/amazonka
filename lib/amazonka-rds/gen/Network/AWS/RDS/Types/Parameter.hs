{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Parameter
  ( Parameter (..)
  -- * Smart constructor
  , mkParameter
  -- * Lenses
  , pAllowedValues
  , pApplyMethod
  , pApplyType
  , pDataType
  , pDescription
  , pIsModifiable
  , pMinimumEngineVersion
  , pParameterName
  , pParameterValue
  , pSource
  , pSupportedEngineModes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.ApplyMethod as Types

-- | This data type is used as a request parameter in the @ModifyDBParameterGroup@ and @ResetDBParameterGroup@ actions. 
--
-- This data type is used as a response element in the @DescribeEngineDefaultParameters@ and @DescribeDBParameters@ actions.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { allowedValues :: Core.Maybe Core.Text
    -- ^ Specifies the valid range of values for the parameter.
  , applyMethod :: Core.Maybe Types.ApplyMethod
    -- ^ Indicates when to apply parameter updates.
  , applyType :: Core.Maybe Core.Text
    -- ^ Specifies the engine specific parameters type.
  , dataType :: Core.Maybe Core.Text
    -- ^ Specifies the valid data type for the parameter.
  , description :: Core.Maybe Core.Text
    -- ^ Provides a description of the parameter.
  , isModifiable :: Core.Maybe Core.Bool
    -- ^ Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed. 
  , minimumEngineVersion :: Core.Maybe Core.Text
    -- ^ The earliest engine version to which the parameter can apply.
  , parameterName :: Core.Maybe Core.Text
    -- ^ Specifies the name of the parameter.
  , parameterValue :: Core.Maybe Core.Text
    -- ^ Specifies the value of the parameter.
  , source :: Core.Maybe Core.Text
    -- ^ Indicates the source of the parameter value.
  , supportedEngineModes :: Core.Maybe [Core.Text]
    -- ^ The valid DB engine modes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parameter' value with any optional fields omitted.
mkParameter
    :: Parameter
mkParameter
  = Parameter'{allowedValues = Core.Nothing,
               applyMethod = Core.Nothing, applyType = Core.Nothing,
               dataType = Core.Nothing, description = Core.Nothing,
               isModifiable = Core.Nothing, minimumEngineVersion = Core.Nothing,
               parameterName = Core.Nothing, parameterValue = Core.Nothing,
               source = Core.Nothing, supportedEngineModes = Core.Nothing}

-- | Specifies the valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pAllowedValues = Lens.field @"allowedValues"
{-# INLINEABLE pAllowedValues #-}
{-# DEPRECATED allowedValues "Use generic-lens or generic-optics with 'allowedValues' instead"  #-}

-- | Indicates when to apply parameter updates.
--
-- /Note:/ Consider using 'applyMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyMethod :: Lens.Lens' Parameter (Core.Maybe Types.ApplyMethod)
pApplyMethod = Lens.field @"applyMethod"
{-# INLINEABLE pApplyMethod #-}
{-# DEPRECATED applyMethod "Use generic-lens or generic-optics with 'applyMethod' instead"  #-}

-- | Specifies the engine specific parameters type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pApplyType = Lens.field @"applyType"
{-# INLINEABLE pApplyType #-}
{-# DEPRECATED applyType "Use generic-lens or generic-optics with 'applyType' instead"  #-}

-- | Specifies the valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pDataType = Lens.field @"dataType"
{-# INLINEABLE pDataType #-}
{-# DEPRECATED dataType "Use generic-lens or generic-optics with 'dataType' instead"  #-}

-- | Provides a description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pDescription = Lens.field @"description"
{-# INLINEABLE pDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed. 
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsModifiable :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
pIsModifiable = Lens.field @"isModifiable"
{-# INLINEABLE pIsModifiable #-}
{-# DEPRECATED isModifiable "Use generic-lens or generic-optics with 'isModifiable' instead"  #-}

-- | The earliest engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMinimumEngineVersion :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pMinimumEngineVersion = Lens.field @"minimumEngineVersion"
{-# INLINEABLE pMinimumEngineVersion #-}
{-# DEPRECATED minimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead"  #-}

-- | Specifies the name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pParameterName = Lens.field @"parameterName"
{-# INLINEABLE pParameterName #-}
{-# DEPRECATED parameterName "Use generic-lens or generic-optics with 'parameterName' instead"  #-}

-- | Specifies the value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pParameterValue = Lens.field @"parameterValue"
{-# INLINEABLE pParameterValue #-}
{-# DEPRECATED parameterValue "Use generic-lens or generic-optics with 'parameterValue' instead"  #-}

-- | Indicates the source of the parameter value.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Core.Maybe Core.Text)
pSource = Lens.field @"source"
{-# INLINEABLE pSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The valid DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSupportedEngineModes :: Lens.Lens' Parameter (Core.Maybe [Core.Text])
pSupportedEngineModes = Lens.field @"supportedEngineModes"
{-# INLINEABLE pSupportedEngineModes #-}
{-# DEPRECATED supportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead"  #-}

instance Core.ToQuery Parameter where
        toQuery Parameter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AllowedValues")
              allowedValues
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyMethod") applyMethod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyType") applyType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DataType") dataType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IsModifiable")
                isModifiable
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinimumEngineVersion")
                minimumEngineVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ParameterName")
                parameterName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ParameterValue")
                parameterValue
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Source") source
              Core.<>
              Core.toQueryPair "SupportedEngineModes"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   supportedEngineModes)

instance Core.FromXML Parameter where
        parseXML x
          = Parameter' Core.<$>
              (x Core..@? "AllowedValues") Core.<*> x Core..@? "ApplyMethod"
                Core.<*> x Core..@? "ApplyType"
                Core.<*> x Core..@? "DataType"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "IsModifiable"
                Core.<*> x Core..@? "MinimumEngineVersion"
                Core.<*> x Core..@? "ParameterName"
                Core.<*> x Core..@? "ParameterValue"
                Core.<*> x Core..@? "Source"
                Core.<*>
                x Core..@? "SupportedEngineModes" Core..<@>
                  Core.parseXMLList "member"
