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
-- Module      : Amazonka.SSM.Types.Parameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Parameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ParameterType

-- | An Amazon Web Services Systems Manager parameter in Parameter Store.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The Amazon Resource Name (ARN) of the parameter.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Date the parameter was last changed or updated and the parameter version
    -- was created.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Either the version number or the label used to retrieve the parameter
    -- value. Specify selectors by using one of the following formats:
    --
    -- parameter_name:version
    --
    -- parameter_name:label
    selector :: Prelude.Maybe Prelude.Text,
    -- | Applies to parameters that reference information in other Amazon Web
    -- Services services. @SourceResult@ is the raw result or response from the
    -- source.
    sourceResult :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    name :: Prelude.Text,
    -- | The type of parameter. Valid values include the following: @String@,
    -- @StringList@, and @SecureString@.
    --
    -- If type is @StringList@, the system returns a comma-separated string
    -- with no spaces between commas in the @Value@ field.
    type' :: ParameterType,
    -- | The parameter value.
    --
    -- If type is @StringList@, the system returns a comma-separated string
    -- with no spaces between commas in the @Value@ field.
    value :: Data.Sensitive Prelude.Text,
    -- | The parameter version.
    version :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'parameter_arn' - The Amazon Resource Name (ARN) of the parameter.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
--
-- 'lastModifiedDate', 'parameter_lastModifiedDate' - Date the parameter was last changed or updated and the parameter version
-- was created.
--
-- 'selector', 'parameter_selector' - Either the version number or the label used to retrieve the parameter
-- value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
--
-- parameter_name:label
--
-- 'sourceResult', 'parameter_sourceResult' - Applies to parameters that reference information in other Amazon Web
-- Services services. @SourceResult@ is the raw result or response from the
-- source.
--
-- 'name', 'parameter_name' - The name of the parameter.
--
-- 'type'', 'parameter_type' - The type of parameter. Valid values include the following: @String@,
-- @StringList@, and @SecureString@.
--
-- If type is @StringList@, the system returns a comma-separated string
-- with no spaces between commas in the @Value@ field.
--
-- 'value', 'parameter_value' - The parameter value.
--
-- If type is @StringList@, the system returns a comma-separated string
-- with no spaces between commas in the @Value@ field.
--
-- 'version', 'parameter_version' - The parameter version.
newParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ParameterType ->
  -- | 'value'
  Prelude.Text ->
  -- | 'version'
  Prelude.Integer ->
  Parameter
newParameter pName_ pType_ pValue_ pVersion_ =
  Parameter'
    { arn = Prelude.Nothing,
      dataType = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      selector = Prelude.Nothing,
      sourceResult = Prelude.Nothing,
      name = pName_,
      type' = pType_,
      value = Data._Sensitive Lens.# pValue_,
      version = pVersion_
    }

-- | The Amazon Resource Name (ARN) of the parameter.
parameter_arn :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_arn = Lens.lens (\Parameter' {arn} -> arn) (\s@Parameter' {} a -> s {arn = a} :: Parameter)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Date the parameter was last changed or updated and the parameter version
-- was created.
parameter_lastModifiedDate :: Lens.Lens' Parameter (Prelude.Maybe Prelude.UTCTime)
parameter_lastModifiedDate = Lens.lens (\Parameter' {lastModifiedDate} -> lastModifiedDate) (\s@Parameter' {} a -> s {lastModifiedDate = a} :: Parameter) Prelude.. Lens.mapping Data._Time

-- | Either the version number or the label used to retrieve the parameter
-- value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
--
-- parameter_name:label
parameter_selector :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_selector = Lens.lens (\Parameter' {selector} -> selector) (\s@Parameter' {} a -> s {selector = a} :: Parameter)

-- | Applies to parameters that reference information in other Amazon Web
-- Services services. @SourceResult@ is the raw result or response from the
-- source.
parameter_sourceResult :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_sourceResult = Lens.lens (\Parameter' {sourceResult} -> sourceResult) (\s@Parameter' {} a -> s {sourceResult = a} :: Parameter)

-- | The name of the parameter.
parameter_name :: Lens.Lens' Parameter Prelude.Text
parameter_name = Lens.lens (\Parameter' {name} -> name) (\s@Parameter' {} a -> s {name = a} :: Parameter)

-- | The type of parameter. Valid values include the following: @String@,
-- @StringList@, and @SecureString@.
--
-- If type is @StringList@, the system returns a comma-separated string
-- with no spaces between commas in the @Value@ field.
parameter_type :: Lens.Lens' Parameter ParameterType
parameter_type = Lens.lens (\Parameter' {type'} -> type') (\s@Parameter' {} a -> s {type' = a} :: Parameter)

-- | The parameter value.
--
-- If type is @StringList@, the system returns a comma-separated string
-- with no spaces between commas in the @Value@ field.
parameter_value :: Lens.Lens' Parameter Prelude.Text
parameter_value = Lens.lens (\Parameter' {value} -> value) (\s@Parameter' {} a -> s {value = a} :: Parameter) Prelude.. Data._Sensitive

-- | The parameter version.
parameter_version :: Lens.Lens' Parameter Prelude.Integer
parameter_version = Lens.lens (\Parameter' {version} -> version) (\s@Parameter' {} a -> s {version = a} :: Parameter)

instance Data.FromJSON Parameter where
  parseJSON =
    Data.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "DataType")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "Selector")
            Prelude.<*> (x Data..:? "SourceResult")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Value")
            Prelude.<*> (x Data..: "Version")
      )

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` selector
      `Prelude.hashWithSalt` sourceResult
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` version

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf selector
      `Prelude.seq` Prelude.rnf sourceResult
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf version
