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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ParameterDeclaration where

import Amazonka.CloudFormation.Types.ParameterConstraints
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ParameterDeclaration data type.
--
-- /See:/ 'newParameterDeclaration' smart constructor.
data ParameterDeclaration = ParameterDeclaration'
  { -- | The default value of the parameter.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The description that\'s associate with the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Flag that indicates whether the parameter value is shown as plain text
    -- in logs and in the Amazon Web Services Management Console.
    noEcho :: Prelude.Maybe Prelude.Bool,
    -- | The criteria that CloudFormation uses to validate parameter values.
    parameterConstraints :: Prelude.Maybe ParameterConstraints,
    -- | The name that\'s associated with the parameter.
    parameterKey :: Prelude.Maybe Prelude.Text,
    -- | The type of parameter.
    parameterType :: Prelude.Maybe Prelude.Text
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
-- 'defaultValue', 'parameterDeclaration_defaultValue' - The default value of the parameter.
--
-- 'description', 'parameterDeclaration_description' - The description that\'s associate with the parameter.
--
-- 'noEcho', 'parameterDeclaration_noEcho' - Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the Amazon Web Services Management Console.
--
-- 'parameterConstraints', 'parameterDeclaration_parameterConstraints' - The criteria that CloudFormation uses to validate parameter values.
--
-- 'parameterKey', 'parameterDeclaration_parameterKey' - The name that\'s associated with the parameter.
--
-- 'parameterType', 'parameterDeclaration_parameterType' - The type of parameter.
newParameterDeclaration ::
  ParameterDeclaration
newParameterDeclaration =
  ParameterDeclaration'
    { defaultValue =
        Prelude.Nothing,
      description = Prelude.Nothing,
      noEcho = Prelude.Nothing,
      parameterConstraints = Prelude.Nothing,
      parameterKey = Prelude.Nothing,
      parameterType = Prelude.Nothing
    }

-- | The default value of the parameter.
parameterDeclaration_defaultValue :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_defaultValue = Lens.lens (\ParameterDeclaration' {defaultValue} -> defaultValue) (\s@ParameterDeclaration' {} a -> s {defaultValue = a} :: ParameterDeclaration)

-- | The description that\'s associate with the parameter.
parameterDeclaration_description :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_description = Lens.lens (\ParameterDeclaration' {description} -> description) (\s@ParameterDeclaration' {} a -> s {description = a} :: ParameterDeclaration)

-- | Flag that indicates whether the parameter value is shown as plain text
-- in logs and in the Amazon Web Services Management Console.
parameterDeclaration_noEcho :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Bool)
parameterDeclaration_noEcho = Lens.lens (\ParameterDeclaration' {noEcho} -> noEcho) (\s@ParameterDeclaration' {} a -> s {noEcho = a} :: ParameterDeclaration)

-- | The criteria that CloudFormation uses to validate parameter values.
parameterDeclaration_parameterConstraints :: Lens.Lens' ParameterDeclaration (Prelude.Maybe ParameterConstraints)
parameterDeclaration_parameterConstraints = Lens.lens (\ParameterDeclaration' {parameterConstraints} -> parameterConstraints) (\s@ParameterDeclaration' {} a -> s {parameterConstraints = a} :: ParameterDeclaration)

-- | The name that\'s associated with the parameter.
parameterDeclaration_parameterKey :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_parameterKey = Lens.lens (\ParameterDeclaration' {parameterKey} -> parameterKey) (\s@ParameterDeclaration' {} a -> s {parameterKey = a} :: ParameterDeclaration)

-- | The type of parameter.
parameterDeclaration_parameterType :: Lens.Lens' ParameterDeclaration (Prelude.Maybe Prelude.Text)
parameterDeclaration_parameterType = Lens.lens (\ParameterDeclaration' {parameterType} -> parameterType) (\s@ParameterDeclaration' {} a -> s {parameterType = a} :: ParameterDeclaration)

instance Data.FromXML ParameterDeclaration where
  parseXML x =
    ParameterDeclaration'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "NoEcho")
      Prelude.<*> (x Data..@? "ParameterConstraints")
      Prelude.<*> (x Data..@? "ParameterKey")
      Prelude.<*> (x Data..@? "ParameterType")

instance Prelude.Hashable ParameterDeclaration where
  hashWithSalt _salt ParameterDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` noEcho
      `Prelude.hashWithSalt` parameterConstraints
      `Prelude.hashWithSalt` parameterKey
      `Prelude.hashWithSalt` parameterType

instance Prelude.NFData ParameterDeclaration where
  rnf ParameterDeclaration' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf noEcho
      `Prelude.seq` Prelude.rnf parameterConstraints
      `Prelude.seq` Prelude.rnf parameterKey
      `Prelude.seq` Prelude.rnf parameterType
