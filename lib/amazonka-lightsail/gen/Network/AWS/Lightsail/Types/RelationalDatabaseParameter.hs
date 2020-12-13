{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseParameter
  ( RelationalDatabaseParameter (..),

    -- * Smart constructor
    mkRelationalDatabaseParameter,

    -- * Lenses
    rdpApplyType,
    rdpParameterValue,
    rdpApplyMethod,
    rdpDataType,
    rdpIsModifiable,
    rdpAllowedValues,
    rdpParameterName,
    rdpDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the parameters of a database.
--
-- /See:/ 'mkRelationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { -- | Specifies the engine-specific parameter type.
    applyType :: Lude.Maybe Lude.Text,
    -- | Specifies the value of the parameter.
    parameterValue :: Lude.Maybe Lude.Text,
    -- | Indicates when parameter updates are applied.
    --
    -- Can be @immediate@ or @pending-reboot@ .
    applyMethod :: Lude.Maybe Lude.Text,
    -- | Specifies the valid data type for the parameter.
    dataType :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating whether the parameter can be modified.
    isModifiable :: Lude.Maybe Lude.Bool,
    -- | Specifies the valid range of values for the parameter.
    allowedValues :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the parameter.
    parameterName :: Lude.Maybe Lude.Text,
    -- | Provides a description of the parameter.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabaseParameter' with the minimum fields required to make a request.
--
-- * 'applyType' - Specifies the engine-specific parameter type.
-- * 'parameterValue' - Specifies the value of the parameter.
-- * 'applyMethod' - Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@ .
-- * 'dataType' - Specifies the valid data type for the parameter.
-- * 'isModifiable' - A Boolean value indicating whether the parameter can be modified.
-- * 'allowedValues' - Specifies the valid range of values for the parameter.
-- * 'parameterName' - Specifies the name of the parameter.
-- * 'description' - Provides a description of the parameter.
mkRelationalDatabaseParameter ::
  RelationalDatabaseParameter
mkRelationalDatabaseParameter =
  RelationalDatabaseParameter'
    { applyType = Lude.Nothing,
      parameterValue = Lude.Nothing,
      applyMethod = Lude.Nothing,
      dataType = Lude.Nothing,
      isModifiable = Lude.Nothing,
      allowedValues = Lude.Nothing,
      parameterName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Specifies the engine-specific parameter type.
--
-- /Note:/ Consider using 'applyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpApplyType :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpApplyType = Lens.lens (applyType :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {applyType = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpApplyType "Use generic-lens or generic-optics with 'applyType' instead." #-}

-- | Specifies the value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpParameterValue :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpParameterValue = Lens.lens (parameterValue :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterValue = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | Indicates when parameter updates are applied.
--
-- Can be @immediate@ or @pending-reboot@ .
--
-- /Note:/ Consider using 'applyMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpApplyMethod :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpApplyMethod = Lens.lens (applyMethod :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {applyMethod = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpApplyMethod "Use generic-lens or generic-optics with 'applyMethod' instead." #-}

-- | Specifies the valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDataType :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpDataType = Lens.lens (dataType :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | A Boolean value indicating whether the parameter can be modified.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpIsModifiable :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Bool)
rdpIsModifiable = Lens.lens (isModifiable :: RelationalDatabaseParameter -> Lude.Maybe Lude.Bool) (\s a -> s {isModifiable = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | Specifies the valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpAllowedValues :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpAllowedValues = Lens.lens (allowedValues :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | Specifies the name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpParameterName :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpParameterName = Lens.lens (parameterName :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterName = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Provides a description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDescription :: Lens.Lens' RelationalDatabaseParameter (Lude.Maybe Lude.Text)
rdpDescription = Lens.lens (description :: RelationalDatabaseParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RelationalDatabaseParameter)
{-# DEPRECATED rdpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON RelationalDatabaseParameter where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseParameter"
      ( \x ->
          RelationalDatabaseParameter'
            Lude.<$> (x Lude..:? "applyType")
            Lude.<*> (x Lude..:? "parameterValue")
            Lude.<*> (x Lude..:? "applyMethod")
            Lude.<*> (x Lude..:? "dataType")
            Lude.<*> (x Lude..:? "isModifiable")
            Lude.<*> (x Lude..:? "allowedValues")
            Lude.<*> (x Lude..:? "parameterName")
            Lude.<*> (x Lude..:? "description")
      )

instance Lude.ToJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("applyType" Lude..=) Lude.<$> applyType,
            ("parameterValue" Lude..=) Lude.<$> parameterValue,
            ("applyMethod" Lude..=) Lude.<$> applyMethod,
            ("dataType" Lude..=) Lude.<$> dataType,
            ("isModifiable" Lude..=) Lude.<$> isModifiable,
            ("allowedValues" Lude..=) Lude.<$> allowedValues,
            ("parameterName" Lude..=) Lude.<$> parameterName,
            ("description" Lude..=) Lude.<$> description
          ]
      )
