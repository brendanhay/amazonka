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
-- Module      : Amazonka.AppFlow.Types.ZendeskConnectorProfileProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ZendeskConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using Zendesk.
--
-- /See:/ 'newZendeskConnectorProfileProperties' smart constructor.
data ZendeskConnectorProfileProperties = ZendeskConnectorProfileProperties'
  { -- | The location of the Zendesk resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZendeskConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'zendeskConnectorProfileProperties_instanceUrl' - The location of the Zendesk resource.
newZendeskConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  ZendeskConnectorProfileProperties
newZendeskConnectorProfileProperties pInstanceUrl_ =
  ZendeskConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the Zendesk resource.
zendeskConnectorProfileProperties_instanceUrl :: Lens.Lens' ZendeskConnectorProfileProperties Prelude.Text
zendeskConnectorProfileProperties_instanceUrl = Lens.lens (\ZendeskConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@ZendeskConnectorProfileProperties' {} a -> s {instanceUrl = a} :: ZendeskConnectorProfileProperties)

instance
  Data.FromJSON
    ZendeskConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "ZendeskConnectorProfileProperties"
      ( \x ->
          ZendeskConnectorProfileProperties'
            Prelude.<$> (x Data..: "instanceUrl")
      )

instance
  Prelude.Hashable
    ZendeskConnectorProfileProperties
  where
  hashWithSalt
    _salt
    ZendeskConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    ZendeskConnectorProfileProperties
  where
  rnf ZendeskConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance
  Data.ToJSON
    ZendeskConnectorProfileProperties
  where
  toJSON ZendeskConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Data..= instanceUrl)]
      )
