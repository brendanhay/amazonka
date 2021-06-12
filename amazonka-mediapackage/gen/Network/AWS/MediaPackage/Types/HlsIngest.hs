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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.IngestEndpoint

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'newHlsIngest' smart constructor.
data HlsIngest = HlsIngest'
  { -- | A list of endpoints to which the source stream should be sent.
    ingestEndpoints :: Core.Maybe [IngestEndpoint]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  HlsIngest' {ingestEndpoints = Core.Nothing}

-- | A list of endpoints to which the source stream should be sent.
hlsIngest_ingestEndpoints :: Lens.Lens' HlsIngest (Core.Maybe [IngestEndpoint])
hlsIngest_ingestEndpoints = Lens.lens (\HlsIngest' {ingestEndpoints} -> ingestEndpoints) (\s@HlsIngest' {} a -> s {ingestEndpoints = a} :: HlsIngest) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON HlsIngest where
  parseJSON =
    Core.withObject
      "HlsIngest"
      ( \x ->
          HlsIngest'
            Core.<$> (x Core..:? "ingestEndpoints" Core..!= Core.mempty)
      )

instance Core.Hashable HlsIngest

instance Core.NFData HlsIngest
