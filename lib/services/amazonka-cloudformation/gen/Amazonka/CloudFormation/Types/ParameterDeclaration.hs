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
-- Module      : Amazonka.CloudFormation.Types.ParameterDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ParameterDeclaration where

import Amazonka.CloudFormation.Types.ParameterConstraints
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The ParameterDeclaration data type.
--
-- /See:/ 'newParameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
  { -- | The name that is associated with the parameter.
    parameterKey :: Prelude.Maybe Prelude.Text,
    -- | The type of parameter.
    parameterType :: Prelude.Maybe Prelude.Text,
    -- | The criteria that CloudFormation uses to validate parameter values.
    parameterConstraints :: Prelude.Maybe ParameterConstraints,
    -- | The default value of the parameter.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Flag that indicates whether the parameter value is shown as plain text
    -- in logs and in the Amazon Web Services Management Console.
    noEcho :: Prelude.Maybe Prelude.Bool,
    -- | The description that is associate with the parameter.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterKey', 'parameterDeclaration_parameterKey' - The name that is associated with the parameter.
--
-- 'parameterType', 'parameterDeclaration_parameterType' - The type of parameter.
--
-- 'parameterConstraints', 'parameterDeclaration_parameterConstraints' - The criteria that CloudFormation uses to validate parameter values.
--
-- 'defaultValue', 'parameterDeclaration_defaultValue' - The default value of the parameter.
--
-- 'noEcho', 'parameterDeclaration_noEcho' - Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the Amazon Web Services Management Console.
--
-- 'description', 'parameterDeclaration_description' - The description that is associate with the parameter.
newParameterDeclaration ::
  ParameterDeclaration
newParameterDeclaration =
  ParameterDeclaration'
    { parameterKey =
        Prelude.Nothing,
      parameterType = Prelude.Nothing,
      parameterConstraints = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      noEcho = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name that is associated with the parameter.
parameterDeclaration_parameterKey :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_parameterKey = Lens.lens (\ParameterDeclaration' {parameterKey} -> parameterKey) (\s@ParameterDeclaration' {} a -> s {parameterKey = a} :: ParameterDeclaration)

-- | The type of parameter.
parameterDeclaration_parameterType :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_parameterType = Lens.lens (\ParameterDeclaration' {parameterType} -> parameterType) (\s@ParameterDeclaration' {} a -> s {parameterType = a} :: ParameterDeclaration)

-- | The criteria that CloudFormation uses to validate parameter values.
parameterDeclaration_parameterConstraints :: Lens.Lens' ParameterDeclaration (Prelude.Maybe ParameterConstraints)
parameterDeclaration_parameterConstraints = Lens.lens (\ParameterDeclaration' {parameterConstraints} -> parameterConstraints) (\s@ParameterDeclaration' {} a -> s {parameterConstraints = a} :: ParameterDeclaration)

-- | The default value of the parameter.
parameterDeclaration_defaultValue :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_defaultValue = Lens.lens (\ParameterDeclaration' {defaultValue} -> defaultValue) (\s@ParameterDeclaration' {} a -> s {defaultValue = a} :: ParameterDeclaration)

-- | Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the Amazon Web Services Management Console.
parameterDeclaration_noEcho :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Bool)
parameterDeclaration_noEcho = Lens.lens (\ParameterDeclaration' {noEcho} -> noEcho) (\s@ParameterDeclaration' {} a -> s {noEcho = a} :: ParameterDeclaration)

-- | The description that is associate with the parameter.
parameterDeclaration_description :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_description = Lens.lens (\ParameterDeclaration' {description} -> description) (\s@ParameterDeclaration' {} a -> s {description = a} :: ParameterDeclaration)

instance Core.FromXML ParameterDeclaration where
  parseXML x =
    ParameterDeclaration'
      Prelude.<$> (x Core..@? "ParameterKey")
      Prelude.<*> (x Core..@? "ParameterType")
      Prelude.<*> (x Core..@? "ParameterConstraints")
      Prelude.<*> (x Core..@? "DefaultValue")
      Prelude.<*> (x Core..@? "NoEcho")
      Prelude.<*> (x Core..@? "Description")

instance Prelude.Hashable ParameterDeclaration where
  hashWithSalt _salt ParameterDeclaration' {..} =
    _salt `Prelude.hashWithSalt` parameterKey
      `Prelude.hashWithSalt` parameterType
      `Prelude.hashWithSalt` parameterConstraints
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` noEcho
      `Prelude.hashWithSalt` description

instance Prelude.NFData ParameterDeclaration where
  rnf ParameterDeclaration' {..} =
    Prelude.rnf parameterKey
      `Prelude.seq` Prelude.rnf parameterType
      `Prelude.seq` Prelude.rnf parameterConstraints
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf noEcho
      `Prelude.seq` Prelude.rnf description
