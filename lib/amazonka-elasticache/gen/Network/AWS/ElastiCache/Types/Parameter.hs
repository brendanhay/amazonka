{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Parameter
  ( Parameter (..),

    -- * Smart constructor
    mkParameter,

    -- * Lenses
    pParameterValue,
    pMinimumEngineVersion,
    pSource,
    pIsModifiable,
    pDataType,
    pAllowedValues,
    pParameterName,
    pDescription,
    pChangeType,
  )
where

import Network.AWS.ElastiCache.Types.ChangeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an individual setting that controls some aspect of ElastiCache behavior.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { parameterValue :: Lude.Maybe Lude.Text,
    minimumEngineVersion :: Lude.Maybe Lude.Text,
    source :: Lude.Maybe Lude.Text,
    isModifiable :: Lude.Maybe Lude.Bool,
    dataType :: Lude.Maybe Lude.Text,
    allowedValues :: Lude.Maybe Lude.Text,
    parameterName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    changeType :: Lude.Maybe ChangeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- * 'allowedValues' - The valid range of values for the parameter.
-- * 'changeType' - Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
-- * 'dataType' - The valid data type for the parameter.
-- * 'description' - A description of the parameter.
-- * 'isModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
-- * 'minimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
-- * 'parameterName' - The name of the parameter.
-- * 'parameterValue' - The value of the parameter.
-- * 'source' - The source of the parameter.
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { parameterValue = Lude.Nothing,
      minimumEngineVersion = Lude.Nothing,
      source = Lude.Nothing,
      isModifiable = Lude.Nothing,
      dataType = Lude.Nothing,
      allowedValues = Lude.Nothing,
      parameterName = Lude.Nothing,
      description = Lude.Nothing,
      changeType = Lude.Nothing
    }

-- | The value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterValue = Lens.lens (parameterValue :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterValue = a} :: Parameter)
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | The earliest cache engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMinimumEngineVersion :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pMinimumEngineVersion = Lens.lens (minimumEngineVersion :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {minimumEngineVersion = a} :: Parameter)
{-# DEPRECATED pMinimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead." #-}

-- | The source of the parameter.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pSource = Lens.lens (source :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: Parameter)
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIsModifiable :: Lens.Lens' Parameter (Lude.Maybe Lude.Bool)
pIsModifiable = Lens.lens (isModifiable :: Parameter -> Lude.Maybe Lude.Bool) (\s a -> s {isModifiable = a} :: Parameter)
{-# DEPRECATED pIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDataType = Lens.lens (dataType :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: Parameter)
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | The valid range of values for the parameter.
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

-- | A description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDescription = Lens.lens (description :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Parameter)
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pChangeType :: Lens.Lens' Parameter (Lude.Maybe ChangeType)
pChangeType = Lens.lens (changeType :: Parameter -> Lude.Maybe ChangeType) (\s a -> s {changeType = a} :: Parameter)
{-# DEPRECATED pChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

instance Lude.FromXML Parameter where
  parseXML x =
    Parameter'
      Lude.<$> (x Lude..@? "ParameterValue")
      Lude.<*> (x Lude..@? "MinimumEngineVersion")
      Lude.<*> (x Lude..@? "Source")
      Lude.<*> (x Lude..@? "IsModifiable")
      Lude.<*> (x Lude..@? "DataType")
      Lude.<*> (x Lude..@? "AllowedValues")
      Lude.<*> (x Lude..@? "ParameterName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "ChangeType")
