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
-- Module      : Amazonka.AppFlow.Types.ConnectorRuntimeSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorRuntimeSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the connector runtime settings that are
-- required for flow execution.
--
-- /See:/ 'newConnectorRuntimeSetting' smart constructor.
data ConnectorRuntimeSetting = ConnectorRuntimeSetting'
  { -- | Contains default values for the connector runtime setting that are
    -- supplied by the connector.
    connectorSuppliedValueOptions :: Prelude.Maybe [Prelude.Text],
    -- | Data type of the connector runtime setting.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | A description about the connector runtime setting.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this connector runtime setting is required.
    isRequired :: Prelude.Maybe Prelude.Bool,
    -- | Contains value information about the connector runtime setting.
    key :: Prelude.Maybe Prelude.Text,
    -- | A label used for connector runtime setting.
    label :: Prelude.Maybe Prelude.Text,
    -- | Indicates the scope of the connector runtime setting.
    scope :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorRuntimeSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorSuppliedValueOptions', 'connectorRuntimeSetting_connectorSuppliedValueOptions' - Contains default values for the connector runtime setting that are
-- supplied by the connector.
--
-- 'dataType', 'connectorRuntimeSetting_dataType' - Data type of the connector runtime setting.
--
-- 'description', 'connectorRuntimeSetting_description' - A description about the connector runtime setting.
--
-- 'isRequired', 'connectorRuntimeSetting_isRequired' - Indicates whether this connector runtime setting is required.
--
-- 'key', 'connectorRuntimeSetting_key' - Contains value information about the connector runtime setting.
--
-- 'label', 'connectorRuntimeSetting_label' - A label used for connector runtime setting.
--
-- 'scope', 'connectorRuntimeSetting_scope' - Indicates the scope of the connector runtime setting.
newConnectorRuntimeSetting ::
  ConnectorRuntimeSetting
newConnectorRuntimeSetting =
  ConnectorRuntimeSetting'
    { connectorSuppliedValueOptions =
        Prelude.Nothing,
      dataType = Prelude.Nothing,
      description = Prelude.Nothing,
      isRequired = Prelude.Nothing,
      key = Prelude.Nothing,
      label = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | Contains default values for the connector runtime setting that are
-- supplied by the connector.
connectorRuntimeSetting_connectorSuppliedValueOptions :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe [Prelude.Text])
connectorRuntimeSetting_connectorSuppliedValueOptions = Lens.lens (\ConnectorRuntimeSetting' {connectorSuppliedValueOptions} -> connectorSuppliedValueOptions) (\s@ConnectorRuntimeSetting' {} a -> s {connectorSuppliedValueOptions = a} :: ConnectorRuntimeSetting) Prelude.. Lens.mapping Lens.coerced

-- | Data type of the connector runtime setting.
connectorRuntimeSetting_dataType :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe Prelude.Text)
connectorRuntimeSetting_dataType = Lens.lens (\ConnectorRuntimeSetting' {dataType} -> dataType) (\s@ConnectorRuntimeSetting' {} a -> s {dataType = a} :: ConnectorRuntimeSetting)

-- | A description about the connector runtime setting.
connectorRuntimeSetting_description :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe Prelude.Text)
connectorRuntimeSetting_description = Lens.lens (\ConnectorRuntimeSetting' {description} -> description) (\s@ConnectorRuntimeSetting' {} a -> s {description = a} :: ConnectorRuntimeSetting)

-- | Indicates whether this connector runtime setting is required.
connectorRuntimeSetting_isRequired :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe Prelude.Bool)
connectorRuntimeSetting_isRequired = Lens.lens (\ConnectorRuntimeSetting' {isRequired} -> isRequired) (\s@ConnectorRuntimeSetting' {} a -> s {isRequired = a} :: ConnectorRuntimeSetting)

-- | Contains value information about the connector runtime setting.
connectorRuntimeSetting_key :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe Prelude.Text)
connectorRuntimeSetting_key = Lens.lens (\ConnectorRuntimeSetting' {key} -> key) (\s@ConnectorRuntimeSetting' {} a -> s {key = a} :: ConnectorRuntimeSetting)

-- | A label used for connector runtime setting.
connectorRuntimeSetting_label :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe Prelude.Text)
connectorRuntimeSetting_label = Lens.lens (\ConnectorRuntimeSetting' {label} -> label) (\s@ConnectorRuntimeSetting' {} a -> s {label = a} :: ConnectorRuntimeSetting)

-- | Indicates the scope of the connector runtime setting.
connectorRuntimeSetting_scope :: Lens.Lens' ConnectorRuntimeSetting (Prelude.Maybe Prelude.Text)
connectorRuntimeSetting_scope = Lens.lens (\ConnectorRuntimeSetting' {scope} -> scope) (\s@ConnectorRuntimeSetting' {} a -> s {scope = a} :: ConnectorRuntimeSetting)

instance Data.FromJSON ConnectorRuntimeSetting where
  parseJSON =
    Data.withObject
      "ConnectorRuntimeSetting"
      ( \x ->
          ConnectorRuntimeSetting'
            Prelude.<$> ( x
                            Data..:? "connectorSuppliedValueOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "dataType")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isRequired")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "label")
            Prelude.<*> (x Data..:? "scope")
      )

instance Prelude.Hashable ConnectorRuntimeSetting where
  hashWithSalt _salt ConnectorRuntimeSetting' {..} =
    _salt
      `Prelude.hashWithSalt` connectorSuppliedValueOptions
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isRequired
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ConnectorRuntimeSetting where
  rnf ConnectorRuntimeSetting' {..} =
    Prelude.rnf connectorSuppliedValueOptions `Prelude.seq`
      Prelude.rnf dataType `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf isRequired `Prelude.seq`
            Prelude.rnf key `Prelude.seq`
              Prelude.rnf label `Prelude.seq`
                Prelude.rnf scope
