{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Parameter
  ( Parameter (..),

    -- * Smart constructor
    mkParameter,

    -- * Lenses
    pAllowedValues,
    pApplyMethod,
    pApplyType,
    pDataType,
    pDescription,
    pIsModifiable,
    pMinimumEngineVersion,
    pParameterName,
    pParameterValue,
    pSource,
    pSupportedEngineModes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.ApplyMethod as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type is used as a request parameter in the @ModifyDBParameterGroup@ and @ResetDBParameterGroup@ actions.
--
-- This data type is used as a response element in the @DescribeEngineDefaultParameters@ and @DescribeDBParameters@ actions.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { -- | Specifies the valid range of values for the parameter.
    allowedValues :: Core.Maybe Types.String,
    -- | Indicates when to apply parameter updates.
    applyMethod :: Core.Maybe Types.ApplyMethod,
    -- | Specifies the engine specific parameters type.
    applyType :: Core.Maybe Types.String,
    -- | Specifies the valid data type for the parameter.
    dataType :: Core.Maybe Types.String,
    -- | Provides a description of the parameter.
    description :: Core.Maybe Types.String,
    -- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
    isModifiable :: Core.Maybe Core.Bool,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Core.Maybe Types.String,
    -- | Specifies the name of the parameter.
    parameterName :: Core.Maybe Types.String,
    -- | Specifies the value of the parameter.
    parameterValue :: Core.Maybe Types.String,
    -- | Indicates the source of the parameter value.
    source :: Core.Maybe Types.String,
    -- | The valid DB engine modes.
    supportedEngineModes :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parameter' value with any optional fields omitted.
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { allowedValues = Core.Nothing,
      applyMethod = Core.Nothing,
      applyType = Core.Nothing,
      dataType = Core.Nothing,
      description = Core.Nothing,
      isModifiable = Core.Nothing,
      minimumEngineVersion = Core.Nothing,
      parameterName = Core.Nothing,
      parameterValue = Core.Nothing,
      source = Core.Nothing,
      supportedEngineModes = Core.Nothing
    }

-- | Specifies the valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Core.Maybe Types.String)
pAllowedValues = Lens.field @"allowedValues"
{-# DEPRECATED pAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | Indicates when to apply parameter updates.
--
-- /Note:/ Consider using 'applyMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyMethod :: Lens.Lens' Parameter (Core.Maybe Types.ApplyMethod)
pApplyMethod = Lens.field @"applyMethod"
{-# DEPRECATED pApplyMethod "Use generic-lens or generic-optics with 'applyMethod' instead." #-}

-- | Specifies the engine specific parameters type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyType :: Lens.Lens' Parameter (Core.Maybe Types.String)
pApplyType = Lens.field @"applyType"
{-# DEPRECATED pApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | Specifies the valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Core.Maybe Types.String)
pDataType = Lens.field @"dataType"
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Provides a description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Core.Maybe Types.String)
pDescription = Lens.field @"description"
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsModifiable :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
pIsModifiable = Lens.field @"isModifiable"
{-# DEPRECATED pIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The earliest engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMinimumEngineVersion :: Lens.Lens' Parameter (Core.Maybe Types.String)
pMinimumEngineVersion = Lens.field @"minimumEngineVersion"
{-# DEPRECATED pMinimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead." #-}

-- | Specifies the name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Core.Maybe Types.String)
pParameterName = Lens.field @"parameterName"
{-# DEPRECATED pParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Specifies the value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Core.Maybe Types.String)
pParameterValue = Lens.field @"parameterValue"
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | Indicates the source of the parameter value.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Core.Maybe Types.String)
pSource = Lens.field @"source"
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The valid DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSupportedEngineModes :: Lens.Lens' Parameter (Core.Maybe [Types.String])
pSupportedEngineModes = Lens.field @"supportedEngineModes"
{-# DEPRECATED pSupportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead." #-}

instance Core.FromXML Parameter where
  parseXML x =
    Parameter'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> (x Core..@? "ApplyMethod")
      Core.<*> (x Core..@? "ApplyType")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "MinimumEngineVersion")
      Core.<*> (x Core..@? "ParameterName")
      Core.<*> (x Core..@? "ParameterValue")
      Core.<*> (x Core..@? "Source")
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..<@> Core.parseXMLList "member"
               )
