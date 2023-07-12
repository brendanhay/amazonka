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
-- Module      : Amazonka.MediaPackage.Types.HlsIngest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.HlsIngest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.IngestEndpoint
import qualified Amazonka.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'newHlsIngest' smart constructor.
data HlsIngest = HlsIngest'
  { -- | A list of endpoints to which the source stream should be sent.
    ingestEndpoints :: Prelude.Maybe [IngestEndpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsIngest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestEndpoints', 'hlsIngest_ingestEndpoints' - A list of endpoints to which the source stream should be sent.
newHlsIngest ::
  HlsIngest
newHlsIngest =
  HlsIngest' {ingestEndpoints = Prelude.Nothing}

-- | A list of endpoints to which the source stream should be sent.
hlsIngest_ingestEndpoints :: Lens.Lens' HlsIngest (Prelude.Maybe [IngestEndpoint])
hlsIngest_ingestEndpoints = Lens.lens (\HlsIngest' {ingestEndpoints} -> ingestEndpoints) (\s@HlsIngest' {} a -> s {ingestEndpoints = a} :: HlsIngest) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON HlsIngest where
  parseJSON =
    Data.withObject
      "HlsIngest"
      ( \x ->
          HlsIngest'
            Prelude.<$> ( x
                            Data..:? "ingestEndpoints"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HlsIngest where
  hashWithSalt _salt HlsIngest' {..} =
    _salt `Prelude.hashWithSalt` ingestEndpoints

instance Prelude.NFData HlsIngest where
  rnf HlsIngest' {..} = Prelude.rnf ingestEndpoints
