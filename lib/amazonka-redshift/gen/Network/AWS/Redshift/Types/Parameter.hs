{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Parameter
  ( Parameter (..),

    -- * Smart constructor
    mkParameter,

    -- * Lenses
    pAllowedValues,
    pApplyType,
    pDataType,
    pDescription,
    pIsModifiable,
    pMinimumEngineVersion,
    pParameterName,
    pParameterValue,
    pSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ParameterApplyType as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes a parameter in a cluster parameter group.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { -- | The valid range of values for the parameter.
    allowedValues :: Core.Maybe Types.String,
    -- | Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
    applyType :: Core.Maybe Types.ParameterApplyType,
    -- | The data type of the parameter.
    dataType :: Core.Maybe Types.String,
    -- | A description of the parameter.
    description :: Core.Maybe Types.String,
    -- | If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
    isModifiable :: Core.Maybe Core.Bool,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Core.Maybe Types.String,
    -- | The name of the parameter.
    parameterName :: Core.Maybe Types.String,
    -- | The value of the parameter.
    parameterValue :: Core.Maybe Types.String,
    -- | The source of the parameter value, such as "engine-default" or "user".
    source :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Parameter' value with any optional fields omitted.
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { allowedValues = Core.Nothing,
      applyType = Core.Nothing,
      dataType = Core.Nothing,
      description = Core.Nothing,
      isModifiable = Core.Nothing,
      minimumEngineVersion = Core.Nothing,
      parameterName = Core.Nothing,
      parameterValue = Core.Nothing,
      source = Core.Nothing
    }

-- | The valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Core.Maybe Types.String)
pAllowedValues = Lens.field @"allowedValues"
{-# DEPRECATED pAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyType :: Lens.Lens' Parameter (Core.Maybe Types.ParameterApplyType)
pApplyType = Lens.field @"applyType"
{-# DEPRECATED pApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | The data type of the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Core.Maybe Types.String)
pDataType = Lens.field @"dataType"
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | A description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Core.Maybe Types.String)
pDescription = Lens.field @"description"
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
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

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Core.Maybe Types.String)
pParameterName = Lens.field @"parameterName"
{-# DEPRECATED pParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | The value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Core.Maybe Types.String)
pParameterValue = Lens.field @"parameterValue"
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | The source of the parameter value, such as "engine-default" or "user".
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Core.Maybe Types.String)
pSource = Lens.field @"source"
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromXML Parameter where
  parseXML x =
    Parameter'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> (x Core..@? "ApplyType")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "MinimumEngineVersion")
      Core.<*> (x Core..@? "ParameterName")
      Core.<*> (x Core..@? "ParameterValue")
      Core.<*> (x Core..@? "Source")
