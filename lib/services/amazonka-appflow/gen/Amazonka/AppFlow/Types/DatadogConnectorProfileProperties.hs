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
-- Module      : Amazonka.AppFlow.Types.DatadogConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DatadogConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required by Datadog.
--
-- /See:/ 'newDatadogConnectorProfileProperties' smart constructor.
data DatadogConnectorProfileProperties = DatadogConnectorProfileProperties'
  { -- | The location of the Datadog resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatadogConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'datadogConnectorProfileProperties_instanceUrl' - The location of the Datadog resource.
newDatadogConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  DatadogConnectorProfileProperties
newDatadogConnectorProfileProperties pInstanceUrl_ =
  DatadogConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the Datadog resource.
datadogConnectorProfileProperties_instanceUrl :: Lens.Lens' DatadogConnectorProfileProperties Prelude.Text
datadogConnectorProfileProperties_instanceUrl = Lens.lens (\DatadogConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@DatadogConnectorProfileProperties' {} a -> s {instanceUrl = a} :: DatadogConnectorProfileProperties)

instance
  Core.FromJSON
    DatadogConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "DatadogConnectorProfileProperties"
      ( \x ->
          DatadogConnectorProfileProperties'
            Prelude.<$> (x Core..: "instanceUrl")
      )

instance
  Prelude.Hashable
    DatadogConnectorProfileProperties
  where
  hashWithSalt
    _salt
    DatadogConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    DatadogConnectorProfileProperties
  where
  rnf DatadogConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance
  Core.ToJSON
    DatadogConnectorProfileProperties
  where
  toJSON DatadogConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Core..= instanceUrl)]
      )
