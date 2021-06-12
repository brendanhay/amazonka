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
import Network.AWS.SSM.Types.ParameterType

-- | An Systems Manager parameter in Parameter Store.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | Date the parameter was last changed or updated and the parameter version
    -- was created.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the parameter.
    arn :: Core.Maybe Core.Text,
    -- | The parameter version.
    version :: Core.Maybe Core.Integer,
    -- | The name of the parameter.
    name :: Core.Maybe Core.Text,
    -- | Applies to parameters that reference information in other AWS services.
    -- SourceResult is the raw result or response from the source.
    sourceResult :: Core.Maybe Core.Text,
    -- | The parameter value.
    value :: Core.Maybe Core.Text,
    -- | The type of parameter. Valid values include the following: @String@,
    -- @StringList@, and @SecureString@.
    type' :: Core.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Core.Maybe Core.Text,
    -- | Either the version number or the label used to retrieve the parameter
    -- value. Specify selectors by using one of the following formats:
    --
    -- parameter_name:version
    --
    -- parameter_name:label
    selector :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'arn', 'parameter_arn' - The Amazon Resource Name (ARN) of the parameter.
--
-- 'version', 'parameter_version' - The parameter version.
--
-- 'name', 'parameter_name' - The name of the parameter.
--
-- 'sourceResult', 'parameter_sourceResult' - Applies to parameters that reference information in other AWS services.
-- SourceResult is the raw result or response from the source.
--
-- 'value', 'parameter_value' - The parameter value.
--
-- 'type'', 'parameter_type' - The type of parameter. Valid values include the following: @String@,
-- @StringList@, and @SecureString@.
--
-- 'dataType', 'parameter_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
--
-- 'selector', 'parameter_selector' - Either the version number or the label used to retrieve the parameter
-- value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
--
-- parameter_name:label
newParameter ::
  Parameter
newParameter =
  Parameter'
    { lastModifiedDate = Core.Nothing,
      arn = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      sourceResult = Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing,
      dataType = Core.Nothing,
      selector = Core.Nothing
    }

-- | Date the parameter was last changed or updated and the parameter version
-- was created.
parameter_lastModifiedDate :: Lens.Lens' Parameter (Core.Maybe Core.UTCTime)
parameter_lastModifiedDate = Lens.lens (\Parameter' {lastModifiedDate} -> lastModifiedDate) (\s@Parameter' {} a -> s {lastModifiedDate = a} :: Parameter) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the parameter.
parameter_arn :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_arn = Lens.lens (\Parameter' {arn} -> arn) (\s@Parameter' {} a -> s {arn = a} :: Parameter)

-- | The parameter version.
parameter_version :: Lens.Lens' Parameter (Core.Maybe Core.Integer)
parameter_version = Lens.lens (\Parameter' {version} -> version) (\s@Parameter' {} a -> s {version = a} :: Parameter)

-- | The name of the parameter.
parameter_name :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_name = Lens.lens (\Parameter' {name} -> name) (\s@Parameter' {} a -> s {name = a} :: Parameter)

-- | Applies to parameters that reference information in other AWS services.
-- SourceResult is the raw result or response from the source.
parameter_sourceResult :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_sourceResult = Lens.lens (\Parameter' {sourceResult} -> sourceResult) (\s@Parameter' {} a -> s {sourceResult = a} :: Parameter)

-- | The parameter value.
parameter_value :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_value = Lens.lens (\Parameter' {value} -> value) (\s@Parameter' {} a -> s {value = a} :: Parameter)

-- | The type of parameter. Valid values include the following: @String@,
-- @StringList@, and @SecureString@.
parameter_type :: Lens.Lens' Parameter (Core.Maybe ParameterType)
parameter_type = Lens.lens (\Parameter' {type'} -> type') (\s@Parameter' {} a -> s {type' = a} :: Parameter)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameter_dataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Either the version number or the label used to retrieve the parameter
-- value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
--
-- parameter_name:label
parameter_selector :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_selector = Lens.lens (\Parameter' {selector} -> selector) (\s@Parameter' {} a -> s {selector = a} :: Parameter)

instance Core.FromJSON Parameter where
  parseJSON =
    Core.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "SourceResult")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "DataType")
            Core.<*> (x Core..:? "Selector")
      )

instance Core.Hashable Parameter

instance Core.NFData Parameter
