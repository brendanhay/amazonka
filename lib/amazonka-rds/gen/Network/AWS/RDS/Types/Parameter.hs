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
    pApplyType,
    pParameterValue,
    pSupportedEngineModes,
    pApplyMethod,
    pMinimumEngineVersion,
    pSource,
    pIsModifiable,
    pDataType,
    pAllowedValues,
    pParameterName,
    pDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.ApplyMethod

-- | This data type is used as a request parameter in the @ModifyDBParameterGroup@ and @ResetDBParameterGroup@ actions.
--
-- This data type is used as a response element in the @DescribeEngineDefaultParameters@ and @DescribeDBParameters@ actions.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { -- | Specifies the engine specific parameters type.
    applyType :: Lude.Maybe Lude.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Lude.Maybe Lude.Text,
    -- | The valid DB engine modes.
    supportedEngineModes :: Lude.Maybe [Lude.Text],
    -- | Indicates when to apply parameter updates.
    applyMethod :: Lude.Maybe ApplyMethod,
    -- | The earliest engine version to which the parameter can apply.
    minimumEngineVersion :: Lude.Maybe Lude.Text,
    -- | Indicates the source of the parameter value.
    source :: Lude.Maybe Lude.Text,
    -- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
    isModifiable :: Lude.Maybe Lude.Bool,
    -- | Specifies the valid data type for the parameter.
    dataType :: Lude.Maybe Lude.Text,
    -- | Specifies the valid range of values for the parameter.
    allowedValues :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Lude.Maybe Lude.Text,
    -- | Provides a description of the parameter.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- * 'applyType' - Specifies the engine specific parameters type.
-- * 'parameterValue' - Specifies the value of the parameter.
-- * 'supportedEngineModes' - The valid DB engine modes.
-- * 'applyMethod' - Indicates when to apply parameter updates.
-- * 'minimumEngineVersion' - The earliest engine version to which the parameter can apply.
-- * 'source' - Indicates the source of the parameter value.
-- * 'isModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
-- * 'dataType' - Specifies the valid data type for the parameter.
-- * 'allowedValues' - Specifies the valid range of values for the parameter.
-- * 'parameterName' - Specifies the name of the parameter.
-- * 'description' - Provides a description of the parameter.
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { applyType = Lude.Nothing,
      parameterValue = Lude.Nothing,
      supportedEngineModes = Lude.Nothing,
      applyMethod = Lude.Nothing,
      minimumEngineVersion = Lude.Nothing,
      source = Lude.Nothing,
      isModifiable = Lude.Nothing,
      dataType = Lude.Nothing,
      allowedValues = Lude.Nothing,
      parameterName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Specifies the engine specific parameters type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyType :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pApplyType = Lens.lens (applyType :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {applyType = a} :: Parameter)
{-# DEPRECATED pApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | Specifies the value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterValue = Lens.lens (parameterValue :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterValue = a} :: Parameter)
{-# DEPRECATED pParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | The valid DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSupportedEngineModes :: Lens.Lens' Parameter (Lude.Maybe [Lude.Text])
pSupportedEngineModes = Lens.lens (supportedEngineModes :: Parameter -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedEngineModes = a} :: Parameter)
{-# DEPRECATED pSupportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead." #-}

-- | Indicates when to apply parameter updates.
--
-- /Note:/ Consider using 'applyMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pApplyMethod :: Lens.Lens' Parameter (Lude.Maybe ApplyMethod)
pApplyMethod = Lens.lens (applyMethod :: Parameter -> Lude.Maybe ApplyMethod) (\s a -> s {applyMethod = a} :: Parameter)
{-# DEPRECATED pApplyMethod "Use generic-lens or generic-optics with 'applyMethod' instead." #-}

-- | The earliest engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMinimumEngineVersion :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pMinimumEngineVersion = Lens.lens (minimumEngineVersion :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {minimumEngineVersion = a} :: Parameter)
{-# DEPRECATED pMinimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead." #-}

-- | Indicates the source of the parameter value.
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

-- | Specifies the valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDataType = Lens.lens (dataType :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: Parameter)
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Specifies the valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowedValues :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pAllowedValues = Lens.lens (allowedValues :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: Parameter)
{-# DEPRECATED pAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | Specifies the name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameterName :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pParameterName = Lens.lens (parameterName :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterName = a} :: Parameter)
{-# DEPRECATED pParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Provides a description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDescription = Lens.lens (description :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Parameter)
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML Parameter where
  parseXML x =
    Parameter'
      Lude.<$> (x Lude..@? "ApplyType")
      Lude.<*> (x Lude..@? "ParameterValue")
      Lude.<*> ( x Lude..@? "SupportedEngineModes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ApplyMethod")
      Lude.<*> (x Lude..@? "MinimumEngineVersion")
      Lude.<*> (x Lude..@? "Source")
      Lude.<*> (x Lude..@? "IsModifiable")
      Lude.<*> (x Lude..@? "DataType")
      Lude.<*> (x Lude..@? "AllowedValues")
      Lude.<*> (x Lude..@? "ParameterName")
      Lude.<*> (x Lude..@? "Description")

instance Lude.ToQuery Parameter where
  toQuery Parameter' {..} =
    Lude.mconcat
      [ "ApplyType" Lude.=: applyType,
        "ParameterValue" Lude.=: parameterValue,
        "SupportedEngineModes"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> supportedEngineModes),
        "ApplyMethod" Lude.=: applyMethod,
        "MinimumEngineVersion" Lude.=: minimumEngineVersion,
        "Source" Lude.=: source,
        "IsModifiable" Lude.=: isModifiable,
        "DataType" Lude.=: dataType,
        "AllowedValues" Lude.=: allowedValues,
        "ParameterName" Lude.=: parameterName,
        "Description" Lude.=: description
      ]
