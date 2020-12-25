{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Parameter
  ( Parameter (..),

    -- * Smart constructor
    mkParameter,

    -- * Lenses
    pAllowedValues,
    pChangeType,
    pDataType,
    pDescription,
    pIsModifiable,
    pNodeTypeSpecificValues,
    pParameterName,
    pParameterType,
    pParameterValue,
    pSource,
  )
where

import qualified Network.AWS.DAX.Types.ChangeType as Types
import qualified Network.AWS.DAX.Types.IsModifiable as Types
import qualified Network.AWS.DAX.Types.NodeTypeSpecificValue as Types
import qualified Network.AWS.DAX.Types.ParameterType as Types
import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an individual setting that controls some aspect of DAX behavior.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { -- | A range of values within which the parameter can be set.
    allowedValues :: Core.Maybe Types.String,
    -- | The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
    changeType :: Core.Maybe Types.ChangeType,
    -- | The data type of the parameter. For example, @integer@ :
    dataType :: Core.Maybe Types.String,
    -- | A description of the parameter
    description :: Core.Maybe Types.String,
    -- | Whether the customer is allowed to modify the parameter.
    isModifiable :: Core.Maybe Types.IsModifiable,
    -- | A list of node types, and specific parameter values for each node.
    nodeTypeSpecificValues :: Core.Maybe [Types.NodeTypeSpecificValue],
    -- | The name of the parameter.
    parameterName :: Core.Maybe Types.String,
    -- | Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
    parameterType :: Core.Maybe Types.ParameterType,
    -- | The value for the parameter.
    parameterValue :: Core.Maybe Types.String,
    -- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
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
      changeType = Core.Nothing,
      dataType = Core.Nothing,
      description = Core.Nothing,
      isModifiable = Core.Nothing,
      nodeTypeSpecificValues = Core.Nothing,
      parameterName = Core.Nothing,
      parameterType = Core.Nothing,
      parameterValue = Core.Nothing,
      source = Core.Nothing
    }

-- | A range of values within which the parameter can be set.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Core.Maybe Types.String)
pAllowedValues = Lens.field @"allowedValues"
{-# DEPRECATED pAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pChangeType :: Lens.Lens' Parameter (Core.Maybe Types.ChangeType)
pChangeType = Lens.field @"changeType"
{-# DEPRECATED pChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

-- | The data type of the parameter. For example, @integer@ :
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Core.Maybe Types.String)
pDataType = Lens.field @"dataType"
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | A description of the parameter
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Core.Maybe Types.String)
pDescription = Lens.field @"description"
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Whether the customer is allowed to modify the parameter.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsModifiable :: Lens.Lens' Parameter (Core.Maybe Types.IsModifiable)
pIsModifiable = Lens.field @"isModifiable"
{-# DEPRECATED pIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | A list of node types, and specific parameter values for each node.
--
-- /Note:/ Consider using 'nodeTypeSpecificValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pNodeTypeSpecificValues :: Lens.Lens' Parameter (Core.Maybe [Types.NodeTypeSpecificValue])
pNodeTypeSpecificValues = Lens.field @"nodeTypeSpecificValues"
{-# DEPRECATED pNodeTypeSpecificValues "Use generic-lens or generic-optics with 'nodeTypeSpecificValues' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Core.Maybe Types.String)
pParameterName = Lens.field @"parameterName"
{-# DEPRECATED pParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
--
-- /Note:/ Consider using 'parameterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterType :: Lens.Lens' Parameter (Core.Maybe Types.ParameterType)
pParameterType = Lens.field @"parameterType"
{-# DEPRECATED pParameterType "Use generic-lens or generic-optics with 'parameterType' instead." #-}

-- | The value for the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Core.Maybe Types.String)
pParameterValue = Lens.field @"parameterValue"
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Core.Maybe Types.String)
pSource = Lens.field @"source"
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject "Parameter" Core.$
      \x ->
        Parameter'
          Core.<$> (x Core..:? "AllowedValues")
          Core.<*> (x Core..:? "ChangeType")
          Core.<*> (x Core..:? "DataType")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IsModifiable")
          Core.<*> (x Core..:? "NodeTypeSpecificValues")
          Core.<*> (x Core..:? "ParameterName")
          Core.<*> (x Core..:? "ParameterType")
          Core.<*> (x Core..:? "ParameterValue")
          Core.<*> (x Core..:? "Source")
