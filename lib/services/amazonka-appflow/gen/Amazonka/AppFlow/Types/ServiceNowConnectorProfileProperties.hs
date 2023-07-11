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
-- Module      : Amazonka.AppFlow.Types.ServiceNowConnectorProfileProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ServiceNowConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using
-- ServiceNow.
--
-- /See:/ 'newServiceNowConnectorProfileProperties' smart constructor.
data ServiceNowConnectorProfileProperties = ServiceNowConnectorProfileProperties'
  { -- | The location of the ServiceNow resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'serviceNowConnectorProfileProperties_instanceUrl' - The location of the ServiceNow resource.
newServiceNowConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  ServiceNowConnectorProfileProperties
newServiceNowConnectorProfileProperties pInstanceUrl_ =
  ServiceNowConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the ServiceNow resource.
serviceNowConnectorProfileProperties_instanceUrl :: Lens.Lens' ServiceNowConnectorProfileProperties Prelude.Text
serviceNowConnectorProfileProperties_instanceUrl = Lens.lens (\ServiceNowConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@ServiceNowConnectorProfileProperties' {} a -> s {instanceUrl = a} :: ServiceNowConnectorProfileProperties)

instance
  Data.FromJSON
    ServiceNowConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "ServiceNowConnectorProfileProperties"
      ( \x ->
          ServiceNowConnectorProfileProperties'
            Prelude.<$> (x Data..: "instanceUrl")
      )

instance
  Prelude.Hashable
    ServiceNowConnectorProfileProperties
  where
  hashWithSalt
    _salt
    ServiceNowConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    ServiceNowConnectorProfileProperties
  where
  rnf ServiceNowConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance
  Data.ToJSON
    ServiceNowConnectorProfileProperties
  where
  toJSON ServiceNowConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Data..= instanceUrl)]
      )
