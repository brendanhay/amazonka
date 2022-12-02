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
-- Module      : Amazonka.AppFlow.Types.InforNexusConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.InforNexusConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required by Infor Nexus.
--
-- /See:/ 'newInforNexusConnectorProfileProperties' smart constructor.
data InforNexusConnectorProfileProperties = InforNexusConnectorProfileProperties'
  { -- | The location of the Infor Nexus resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InforNexusConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'inforNexusConnectorProfileProperties_instanceUrl' - The location of the Infor Nexus resource.
newInforNexusConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  InforNexusConnectorProfileProperties
newInforNexusConnectorProfileProperties pInstanceUrl_ =
  InforNexusConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the Infor Nexus resource.
inforNexusConnectorProfileProperties_instanceUrl :: Lens.Lens' InforNexusConnectorProfileProperties Prelude.Text
inforNexusConnectorProfileProperties_instanceUrl = Lens.lens (\InforNexusConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@InforNexusConnectorProfileProperties' {} a -> s {instanceUrl = a} :: InforNexusConnectorProfileProperties)

instance
  Data.FromJSON
    InforNexusConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "InforNexusConnectorProfileProperties"
      ( \x ->
          InforNexusConnectorProfileProperties'
            Prelude.<$> (x Data..: "instanceUrl")
      )

instance
  Prelude.Hashable
    InforNexusConnectorProfileProperties
  where
  hashWithSalt
    _salt
    InforNexusConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    InforNexusConnectorProfileProperties
  where
  rnf InforNexusConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance
  Data.ToJSON
    InforNexusConnectorProfileProperties
  where
  toJSON InforNexusConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Data..= instanceUrl)]
      )
