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
-- Module      : Amazonka.MediaPackageV2.Types.IngestEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.IngestEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ingest domain URL where the source stream should be sent.
--
-- /See:/ 'newIngestEndpoint' smart constructor.
data IngestEndpoint = IngestEndpoint'
  { -- | The system-generated unique identifier for the IngestEndpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ingest domain URL where the source stream should be sent.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'ingestEndpoint_id' - The system-generated unique identifier for the IngestEndpoint.
--
-- 'url', 'ingestEndpoint_url' - The ingest domain URL where the source stream should be sent.
newIngestEndpoint ::
  IngestEndpoint
newIngestEndpoint =
  IngestEndpoint'
    { id = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The system-generated unique identifier for the IngestEndpoint.
ingestEndpoint_id :: Lens.Lens' IngestEndpoint (Prelude.Maybe Prelude.Text)
ingestEndpoint_id = Lens.lens (\IngestEndpoint' {id} -> id) (\s@IngestEndpoint' {} a -> s {id = a} :: IngestEndpoint)

-- | The ingest domain URL where the source stream should be sent.
ingestEndpoint_url :: Lens.Lens' IngestEndpoint (Prelude.Maybe Prelude.Text)
ingestEndpoint_url = Lens.lens (\IngestEndpoint' {url} -> url) (\s@IngestEndpoint' {} a -> s {url = a} :: IngestEndpoint)

instance Data.FromJSON IngestEndpoint where
  parseJSON =
    Data.withObject
      "IngestEndpoint"
      ( \x ->
          IngestEndpoint'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable IngestEndpoint where
  hashWithSalt _salt IngestEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` url

instance Prelude.NFData IngestEndpoint where
  rnf IngestEndpoint' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf url
