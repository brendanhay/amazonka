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
-- Module      : Network.AWS.SSM.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Parameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.ParameterType

-- | An Amazon Web Services Systems Manager parameter in Parameter Store.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | Date the parameter was last changed or updated and the parameter version
    -- was created.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | Either the version number or the label used to retrieve the parameter
    -- value. Specify selectors by using one of the following formats:
    --
    -- parameter_name:version
    --
    -- parameter_name:label
    selector :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the parameter.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The parameter value.
    value :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Applies to parameters that reference information in other Amazon Web
    -- Services services. @SourceResult@ is the raw result or response from the
    -- source.
    sourceResult :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The parameter version.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The type of parameter. Valid values include the following: @String@,
    -- @StringList@, and @SecureString@.
    type' :: Prelude.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'parameter_arn' - The Amazon Resource Name (ARN) of the parameter.
--
-- 'value', 'parameter_value' - The parameter value.
--
-- 'sourceResult', 'parameter_sourceResult' - Applies to parameters that reference information in other Amazon Web
-- Services services. @SourceResult@ is the raw result or response from the
-- source.
--
-- 'name', 'parameter_name' - The name of the parameter.
--
-- 'version', 'parameter_version' - The parameter version.
--
-- 'type'', 'parameter_type' - The type of parameter. Valid values include the following: @String@,
-- @StringList@, and @SecureString@.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { lastModifiedDate = Prelude.Nothing,
      selector = Prelude.Nothing,
      arn = Prelude.Nothing,
      value = Prelude.Nothing,
      sourceResult = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      type' = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | Date the parameter was last changed or updated and the parameter version
-- was created.
parameter_lastModifiedDate :: Lens.Lens' Parameter (Prelude.Maybe Prelude.UTCTime)
parameter_lastModifiedDate = Lens.lens (\Parameter' {lastModifiedDate} -> lastModifiedDate) (\s@Parameter' {} a -> s {lastModifiedDate = a} :: Parameter) Prelude.. Lens.mapping Core._Time

-- | Either the version number or the label used to retrieve the parameter
-- value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
--
-- parameter_name:label
parameter_selector :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_selector = Lens.lens (\Parameter' {selector} -> selector) (\s@Parameter' {} a -> s {selector = a} :: Parameter)

-- | The Amazon Resource Name (ARN) of the parameter.
parameter_arn :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_arn = Lens.lens (\Parameter' {arn} -> arn) (\s@Parameter' {} a -> s {arn = a} :: Parameter)

-- | The parameter value.
parameter_value :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_value = Lens.lens (\Parameter' {value} -> value) (\s@Parameter' {} a -> s {value = a} :: Parameter) Prelude.. Lens.mapping Core._Sensitive

-- | Applies to parameters that reference information in other Amazon Web
-- Services services. @SourceResult@ is the raw result or response from the
-- source.
parameter_sourceResult :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_sourceResult = Lens.lens (\Parameter' {sourceResult} -> sourceResult) (\s@Parameter' {} a -> s {sourceResult = a} :: Parameter)

-- | The name of the parameter.
parameter_name :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_name = Lens.lens (\Parameter' {name} -> name) (\s@Parameter' {} a -> s {name = a} :: Parameter)

-- | The parameter version.
parameter_version :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Integer)
parameter_version = Lens.lens (\Parameter' {version} -> version) (\s@Parameter' {} a -> s {version = a} :: Parameter)

-- | The type of parameter. Valid values include the following: @String@,
-- @StringList@, and @SecureString@.
parameter_type :: Lens.Lens' Parameter (Prelude.Maybe ParameterType)
parameter_type = Lens.lens (\Parameter' {type'} -> type') (\s@Parameter' {} a -> s {type' = a} :: Parameter)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "Selector")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "SourceResult")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "DataType")
      )

instance Prelude.Hashable Parameter

instance Prelude.NFData Parameter
