{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.Types.HlsIngest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsIngest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.IngestEndpoint
import qualified Network.AWS.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'newHlsIngest' smart constructor.
data HlsIngest = HlsIngest'
  { -- | A list of endpoints to which the source stream should be sent.
    ingestEndpoints :: Prelude.Maybe [IngestEndpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
hlsIngest_ingestEndpoints = Lens.lens (\HlsIngest' {ingestEndpoints} -> ingestEndpoints) (\s@HlsIngest' {} a -> s {ingestEndpoints = a} :: HlsIngest) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON HlsIngest where
  parseJSON =
    Prelude.withObject
      "HlsIngest"
      ( \x ->
          HlsIngest'
            Prelude.<$> ( x Prelude..:? "ingestEndpoints"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HlsIngest

instance Prelude.NFData HlsIngest
