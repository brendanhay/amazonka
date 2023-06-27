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
-- Module      : Amazonka.AppFlow.Types.DataTransferApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DataTransferApi where

import Amazonka.AppFlow.Types.DataTransferApiType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The API of the connector application that Amazon AppFlow uses to
-- transfer your data.
--
-- /See:/ 'newDataTransferApi' smart constructor.
data DataTransferApi = DataTransferApi'
  { -- | The name of the connector application API.
    name :: Prelude.Maybe Prelude.Text,
    -- | You can specify one of the following types:
    --
    -- [AUTOMATIC]
    --     The default. Optimizes a flow for datasets that fluctuate in size
    --     from small to large. For each flow run, Amazon AppFlow chooses to
    --     use the SYNC or ASYNC API type based on the amount of data that the
    --     run transfers.
    --
    -- [SYNC]
    --     A synchronous API. This type of API optimizes a flow for small to
    --     medium-sized datasets.
    --
    -- [ASYNC]
    --     An asynchronous API. This type of API optimizes a flow for large
    --     datasets.
    type' :: Prelude.Maybe DataTransferApiType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataTransferApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dataTransferApi_name' - The name of the connector application API.
--
-- 'type'', 'dataTransferApi_type' - You can specify one of the following types:
--
-- [AUTOMATIC]
--     The default. Optimizes a flow for datasets that fluctuate in size
--     from small to large. For each flow run, Amazon AppFlow chooses to
--     use the SYNC or ASYNC API type based on the amount of data that the
--     run transfers.
--
-- [SYNC]
--     A synchronous API. This type of API optimizes a flow for small to
--     medium-sized datasets.
--
-- [ASYNC]
--     An asynchronous API. This type of API optimizes a flow for large
--     datasets.
newDataTransferApi ::
  DataTransferApi
newDataTransferApi =
  DataTransferApi'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the connector application API.
dataTransferApi_name :: Lens.Lens' DataTransferApi (Prelude.Maybe Prelude.Text)
dataTransferApi_name = Lens.lens (\DataTransferApi' {name} -> name) (\s@DataTransferApi' {} a -> s {name = a} :: DataTransferApi)

-- | You can specify one of the following types:
--
-- [AUTOMATIC]
--     The default. Optimizes a flow for datasets that fluctuate in size
--     from small to large. For each flow run, Amazon AppFlow chooses to
--     use the SYNC or ASYNC API type based on the amount of data that the
--     run transfers.
--
-- [SYNC]
--     A synchronous API. This type of API optimizes a flow for small to
--     medium-sized datasets.
--
-- [ASYNC]
--     An asynchronous API. This type of API optimizes a flow for large
--     datasets.
dataTransferApi_type :: Lens.Lens' DataTransferApi (Prelude.Maybe DataTransferApiType)
dataTransferApi_type = Lens.lens (\DataTransferApi' {type'} -> type') (\s@DataTransferApi' {} a -> s {type' = a} :: DataTransferApi)

instance Data.FromJSON DataTransferApi where
  parseJSON =
    Data.withObject
      "DataTransferApi"
      ( \x ->
          DataTransferApi'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable DataTransferApi where
  hashWithSalt _salt DataTransferApi' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DataTransferApi where
  rnf DataTransferApi' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON DataTransferApi where
  toJSON DataTransferApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
