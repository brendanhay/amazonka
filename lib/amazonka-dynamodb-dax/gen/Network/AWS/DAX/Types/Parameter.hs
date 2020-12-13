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
    pParameterValue,
    pParameterType,
    pSource,
    pIsModifiable,
    pDataType,
    pNodeTypeSpecificValues,
    pAllowedValues,
    pParameterName,
    pDescription,
    pChangeType,
  )
where

import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.IsModifiable
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.ParameterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an individual setting that controls some aspect of DAX behavior.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { -- | The value for the parameter.
    parameterValue :: Lude.Maybe Lude.Text,
    -- | Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
    parameterType :: Lude.Maybe ParameterType,
    -- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
    source :: Lude.Maybe Lude.Text,
    -- | Whether the customer is allowed to modify the parameter.
    isModifiable :: Lude.Maybe IsModifiable,
    -- | The data type of the parameter. For example, @integer@ :
    dataType :: Lude.Maybe Lude.Text,
    -- | A list of node types, and specific parameter values for each node.
    nodeTypeSpecificValues :: Lude.Maybe [NodeTypeSpecificValue],
    -- | A range of values within which the parameter can be set.
    allowedValues :: Lude.Maybe Lude.Text,
    -- | The name of the parameter.
    parameterName :: Lude.Maybe Lude.Text,
    -- | A description of the parameter
    description :: Lude.Maybe Lude.Text,
    -- | The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
    changeType :: Lude.Maybe ChangeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- * 'parameterValue' - The value for the parameter.
-- * 'parameterType' - Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
-- * 'source' - How the parameter is defined. For example, @system@ denotes a system-defined parameter.
-- * 'isModifiable' - Whether the customer is allowed to modify the parameter.
-- * 'dataType' - The data type of the parameter. For example, @integer@ :
-- * 'nodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
-- * 'allowedValues' - A range of values within which the parameter can be set.
-- * 'parameterName' - The name of the parameter.
-- * 'description' - A description of the parameter
-- * 'changeType' - The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { parameterValue = Lude.Nothing,
      parameterType = Lude.Nothing,
      source = Lude.Nothing,
      isModifiable = Lude.Nothing,
      dataType = Lude.Nothing,
      nodeTypeSpecificValues = Lude.Nothing,
      allowedValues = Lude.Nothing,
      parameterName = Lude.Nothing,
      description = Lude.Nothing,
      changeType = Lude.Nothing
    }

-- | The value for the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterValue = Lens.lens (parameterValue :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterValue = a} :: Parameter)
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
--
-- /Note:/ Consider using 'parameterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterType :: Lens.Lens' Parameter (Lude.Maybe ParameterType)
pParameterType = Lens.lens (parameterType :: Parameter -> Lude.Maybe ParameterType) (\s a -> s {parameterType = a} :: Parameter)
{-# DEPRECATED pParameterType "Use generic-lens or generic-optics with 'parameterType' instead." #-}

-- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pSource = Lens.lens (source :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: Parameter)
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Whether the customer is allowed to modify the parameter.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsModifiable :: Lens.Lens' Parameter (Lude.Maybe IsModifiable)
pIsModifiable = Lens.lens (isModifiable :: Parameter -> Lude.Maybe IsModifiable) (\s a -> s {isModifiable = a} :: Parameter)
{-# DEPRECATED pIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The data type of the parameter. For example, @integer@ :
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDataType = Lens.lens (dataType :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: Parameter)
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | A list of node types, and specific parameter values for each node.
--
-- /Note:/ Consider using 'nodeTypeSpecificValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pNodeTypeSpecificValues :: Lens.Lens' Parameter (Lude.Maybe [NodeTypeSpecificValue])
pNodeTypeSpecificValues = Lens.lens (nodeTypeSpecificValues :: Parameter -> Lude.Maybe [NodeTypeSpecificValue]) (\s a -> s {nodeTypeSpecificValues = a} :: Parameter)
{-# DEPRECATED pNodeTypeSpecificValues "Use generic-lens or generic-optics with 'nodeTypeSpecificValues' instead." #-}

-- | A range of values within which the parameter can be set.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pAllowedValues = Lens.lens (allowedValues :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: Parameter)
{-# DEPRECATED pAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterName = Lens.lens (parameterName :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterName = a} :: Parameter)
{-# DEPRECATED pParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | A description of the parameter
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDescription = Lens.lens (description :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Parameter)
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pChangeType :: Lens.Lens' Parameter (Lude.Maybe ChangeType)
pChangeType = Lens.lens (changeType :: Parameter -> Lude.Maybe ChangeType) (\s a -> s {changeType = a} :: Parameter)
{-# DEPRECATED pChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

instance Lude.FromJSON Parameter where
  parseJSON =
    Lude.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Lude.<$> (x Lude..:? "ParameterValue")
            Lude.<*> (x Lude..:? "ParameterType")
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "IsModifiable")
            Lude.<*> (x Lude..:? "DataType")
            Lude.<*> (x Lude..:? "NodeTypeSpecificValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AllowedValues")
            Lude.<*> (x Lude..:? "ParameterName")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ChangeType")
      )
