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
-- Module      : Amazonka.MacieV2.Types.ApiCallDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ApiCallDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an API operation that an entity invoked for
-- an affected resource.
--
-- /See:/ 'newApiCallDetails' smart constructor.
data ApiCallDetails = ApiCallDetails'
  { -- | The most recent date and time, in UTC and extended ISO 8601 format, when
    -- the specified operation (api) was invoked and produced the finding.
    lastSeen :: Prelude.Maybe Core.POSIX,
    -- | The URL of the Amazon Web Service that provides the operation, for
    -- example: s3.amazonaws.com.
    apiServiceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the operation that was invoked most recently and produced
    -- the finding.
    api :: Prelude.Maybe Prelude.Text,
    -- | The first date and time, in UTC and extended ISO 8601 format, when any
    -- operation was invoked and produced the finding.
    firstSeen :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiCallDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastSeen', 'apiCallDetails_lastSeen' - The most recent date and time, in UTC and extended ISO 8601 format, when
-- the specified operation (api) was invoked and produced the finding.
--
-- 'apiServiceName', 'apiCallDetails_apiServiceName' - The URL of the Amazon Web Service that provides the operation, for
-- example: s3.amazonaws.com.
--
-- 'api', 'apiCallDetails_api' - The name of the operation that was invoked most recently and produced
-- the finding.
--
-- 'firstSeen', 'apiCallDetails_firstSeen' - The first date and time, in UTC and extended ISO 8601 format, when any
-- operation was invoked and produced the finding.
newApiCallDetails ::
  ApiCallDetails
newApiCallDetails =
  ApiCallDetails'
    { lastSeen = Prelude.Nothing,
      apiServiceName = Prelude.Nothing,
      api = Prelude.Nothing,
      firstSeen = Prelude.Nothing
    }

-- | The most recent date and time, in UTC and extended ISO 8601 format, when
-- the specified operation (api) was invoked and produced the finding.
apiCallDetails_lastSeen :: Lens.Lens' ApiCallDetails (Prelude.Maybe Prelude.UTCTime)
apiCallDetails_lastSeen = Lens.lens (\ApiCallDetails' {lastSeen} -> lastSeen) (\s@ApiCallDetails' {} a -> s {lastSeen = a} :: ApiCallDetails) Prelude.. Lens.mapping Core._Time

-- | The URL of the Amazon Web Service that provides the operation, for
-- example: s3.amazonaws.com.
apiCallDetails_apiServiceName :: Lens.Lens' ApiCallDetails (Prelude.Maybe Prelude.Text)
apiCallDetails_apiServiceName = Lens.lens (\ApiCallDetails' {apiServiceName} -> apiServiceName) (\s@ApiCallDetails' {} a -> s {apiServiceName = a} :: ApiCallDetails)

-- | The name of the operation that was invoked most recently and produced
-- the finding.
apiCallDetails_api :: Lens.Lens' ApiCallDetails (Prelude.Maybe Prelude.Text)
apiCallDetails_api = Lens.lens (\ApiCallDetails' {api} -> api) (\s@ApiCallDetails' {} a -> s {api = a} :: ApiCallDetails)

-- | The first date and time, in UTC and extended ISO 8601 format, when any
-- operation was invoked and produced the finding.
apiCallDetails_firstSeen :: Lens.Lens' ApiCallDetails (Prelude.Maybe Prelude.UTCTime)
apiCallDetails_firstSeen = Lens.lens (\ApiCallDetails' {firstSeen} -> firstSeen) (\s@ApiCallDetails' {} a -> s {firstSeen = a} :: ApiCallDetails) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ApiCallDetails where
  parseJSON =
    Core.withObject
      "ApiCallDetails"
      ( \x ->
          ApiCallDetails'
            Prelude.<$> (x Core..:? "lastSeen")
            Prelude.<*> (x Core..:? "apiServiceName")
            Prelude.<*> (x Core..:? "api")
            Prelude.<*> (x Core..:? "firstSeen")
      )

instance Prelude.Hashable ApiCallDetails where
  hashWithSalt _salt ApiCallDetails' {..} =
    _salt `Prelude.hashWithSalt` lastSeen
      `Prelude.hashWithSalt` apiServiceName
      `Prelude.hashWithSalt` api
      `Prelude.hashWithSalt` firstSeen

instance Prelude.NFData ApiCallDetails where
  rnf ApiCallDetails' {..} =
    Prelude.rnf lastSeen
      `Prelude.seq` Prelude.rnf apiServiceName
      `Prelude.seq` Prelude.rnf api
      `Prelude.seq` Prelude.rnf firstSeen
