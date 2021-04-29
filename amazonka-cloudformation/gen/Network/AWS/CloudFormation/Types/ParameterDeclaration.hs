{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ParameterDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ParameterDeclaration where

import Network.AWS.CloudFormation.Types.ParameterConstraints
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The ParameterDeclaration data type.
--
-- /See:/ 'newParameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
  { -- | The criteria that AWS CloudFormation uses to validate parameter values.
    parameterConstraints :: Prelude.Maybe ParameterConstraints,
    -- | The type of parameter.
    parameterType :: Prelude.Maybe Prelude.Text,
    -- | The name that is associated with the parameter.
    parameterKey :: Prelude.Maybe Prelude.Text,
    -- | The description that is associate with the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Flag that indicates whether the parameter value is shown as plain text
    -- in logs and in the AWS Management Console.
    noEcho :: Prelude.Maybe Prelude.Bool,
    -- | The default value of the parameter.
    defaultValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterConstraints', 'parameterDeclaration_parameterConstraints' - The criteria that AWS CloudFormation uses to validate parameter values.
--
-- 'parameterType', 'parameterDeclaration_parameterType' - The type of parameter.
--
-- 'parameterKey', 'parameterDeclaration_parameterKey' - The name that is associated with the parameter.
--
-- 'description', 'parameterDeclaration_description' - The description that is associate with the parameter.
--
-- 'noEcho', 'parameterDeclaration_noEcho' - Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the AWS Management Console.
--
-- 'defaultValue', 'parameterDeclaration_defaultValue' - The default value of the parameter.
newParameterDeclaration ::
  ParameterDeclaration
newParameterDeclaration =
  ParameterDeclaration'
    { parameterConstraints =
        Prelude.Nothing,
      parameterType = Prelude.Nothing,
      parameterKey = Prelude.Nothing,
      description = Prelude.Nothing,
      noEcho = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | The criteria that AWS CloudFormation uses to validate parameter values.
parameterDeclaration_parameterConstraints :: Lens.Lens' ParameterDeclaration (Prelude.Maybe ParameterConstraints)
parameterDeclaration_parameterConstraints = Lens.lens (\ParameterDeclaration' {parameterConstraints} -> parameterConstraints) (\s@ParameterDeclaration' {} a -> s {parameterConstraints = a} :: ParameterDeclaration)

-- | The type of parameter.
parameterDeclaration_parameterType :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_parameterType = Lens.lens (\ParameterDeclaration' {parameterType} -> parameterType) (\s@ParameterDeclaration' {} a -> s {parameterType = a} :: ParameterDeclaration)

-- | The name that is associated with the parameter.
parameterDeclaration_parameterKey :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_parameterKey = Lens.lens (\ParameterDeclaration' {parameterKey} -> parameterKey) (\s@ParameterDeclaration' {} a -> s {parameterKey = a} :: ParameterDeclaration)

-- | The description that is associate with the parameter.
parameterDeclaration_description :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_description = Lens.lens (\ParameterDeclaration' {description} -> description) (\s@ParameterDeclaration' {} a -> s {description = a} :: ParameterDeclaration)

-- | Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the AWS Management Console.
parameterDeclaration_noEcho :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Bool)
parameterDeclaration_noEcho = Lens.lens (\ParameterDeclaration' {noEcho} -> noEcho) (\s@ParameterDeclaration' {} a -> s {noEcho = a} :: ParameterDeclaration)

-- | The default value of the parameter.
parameterDeclaration_defaultValue :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_defaultValue = Lens.lens (\ParameterDeclaration' {defaultValue} -> defaultValue) (\s@ParameterDeclaration' {} a -> s {defaultValue = a} :: ParameterDeclaration)

instance Prelude.FromXML ParameterDeclaration where
  parseXML x =
    ParameterDeclaration'
      Prelude.<$> (x Prelude..@? "ParameterConstraints")
      Prelude.<*> (x Prelude..@? "ParameterType")
      Prelude.<*> (x Prelude..@? "ParameterKey")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "NoEcho")
      Prelude.<*> (x Prelude..@? "DefaultValue")

instance Prelude.Hashable ParameterDeclaration

instance Prelude.NFData ParameterDeclaration
