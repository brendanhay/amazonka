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
-- Module      : Amazonka.MachineLearning.Types.RealtimeEndpointInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RealtimeEndpointInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types.RealtimeEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the real-time endpoint information for an @MLModel@.
--
-- /See:/ 'newRealtimeEndpointInfo' smart constructor.
data RealtimeEndpointInfo = RealtimeEndpointInfo'
  { -- | The time that the request to create the real-time endpoint for the
    -- @MLModel@ was received. The time is expressed in epoch time.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The current status of the real-time endpoint for the @MLModel@. This
    -- element can have one of the following values:
    --
    -- -   @NONE@ - Endpoint does not exist or was previously deleted.
    --
    -- -   @READY@ - Endpoint is ready to be used for real-time predictions.
    --
    -- -   @UPDATING@ - Updating\/creating the endpoint.
    endpointStatus :: Prelude.Maybe RealtimeEndpointStatus,
    -- | The URI that specifies where to send real-time prediction requests for
    -- the @MLModel@.
    --
    -- __Note:__ The application must wait until the real-time endpoint is
    -- ready before using this URI.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The maximum processing rate for the real-time endpoint for @MLModel@,
    -- measured in incoming requests per second.
    peakRequestsPerSecond :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealtimeEndpointInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'realtimeEndpointInfo_createdAt' - The time that the request to create the real-time endpoint for the
-- @MLModel@ was received. The time is expressed in epoch time.
--
-- 'endpointStatus', 'realtimeEndpointInfo_endpointStatus' - The current status of the real-time endpoint for the @MLModel@. This
-- element can have one of the following values:
--
-- -   @NONE@ - Endpoint does not exist or was previously deleted.
--
-- -   @READY@ - Endpoint is ready to be used for real-time predictions.
--
-- -   @UPDATING@ - Updating\/creating the endpoint.
--
-- 'endpointUrl', 'realtimeEndpointInfo_endpointUrl' - The URI that specifies where to send real-time prediction requests for
-- the @MLModel@.
--
-- __Note:__ The application must wait until the real-time endpoint is
-- ready before using this URI.
--
-- 'peakRequestsPerSecond', 'realtimeEndpointInfo_peakRequestsPerSecond' - The maximum processing rate for the real-time endpoint for @MLModel@,
-- measured in incoming requests per second.
newRealtimeEndpointInfo ::
  RealtimeEndpointInfo
newRealtimeEndpointInfo =
  RealtimeEndpointInfo'
    { createdAt = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      peakRequestsPerSecond = Prelude.Nothing
    }

-- | The time that the request to create the real-time endpoint for the
-- @MLModel@ was received. The time is expressed in epoch time.
realtimeEndpointInfo_createdAt :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe Prelude.UTCTime)
realtimeEndpointInfo_createdAt = Lens.lens (\RealtimeEndpointInfo' {createdAt} -> createdAt) (\s@RealtimeEndpointInfo' {} a -> s {createdAt = a} :: RealtimeEndpointInfo) Prelude.. Lens.mapping Data._Time

-- | The current status of the real-time endpoint for the @MLModel@. This
-- element can have one of the following values:
--
-- -   @NONE@ - Endpoint does not exist or was previously deleted.
--
-- -   @READY@ - Endpoint is ready to be used for real-time predictions.
--
-- -   @UPDATING@ - Updating\/creating the endpoint.
realtimeEndpointInfo_endpointStatus :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe RealtimeEndpointStatus)
realtimeEndpointInfo_endpointStatus = Lens.lens (\RealtimeEndpointInfo' {endpointStatus} -> endpointStatus) (\s@RealtimeEndpointInfo' {} a -> s {endpointStatus = a} :: RealtimeEndpointInfo)

-- | The URI that specifies where to send real-time prediction requests for
-- the @MLModel@.
--
-- __Note:__ The application must wait until the real-time endpoint is
-- ready before using this URI.
realtimeEndpointInfo_endpointUrl :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe Prelude.Text)
realtimeEndpointInfo_endpointUrl = Lens.lens (\RealtimeEndpointInfo' {endpointUrl} -> endpointUrl) (\s@RealtimeEndpointInfo' {} a -> s {endpointUrl = a} :: RealtimeEndpointInfo)

-- | The maximum processing rate for the real-time endpoint for @MLModel@,
-- measured in incoming requests per second.
realtimeEndpointInfo_peakRequestsPerSecond :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe Prelude.Int)
realtimeEndpointInfo_peakRequestsPerSecond = Lens.lens (\RealtimeEndpointInfo' {peakRequestsPerSecond} -> peakRequestsPerSecond) (\s@RealtimeEndpointInfo' {} a -> s {peakRequestsPerSecond = a} :: RealtimeEndpointInfo)

instance Data.FromJSON RealtimeEndpointInfo where
  parseJSON =
    Data.withObject
      "RealtimeEndpointInfo"
      ( \x ->
          RealtimeEndpointInfo'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "EndpointStatus")
            Prelude.<*> (x Data..:? "EndpointUrl")
            Prelude.<*> (x Data..:? "PeakRequestsPerSecond")
      )

instance Prelude.Hashable RealtimeEndpointInfo where
  hashWithSalt _salt RealtimeEndpointInfo' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` endpointUrl
      `Prelude.hashWithSalt` peakRequestsPerSecond

instance Prelude.NFData RealtimeEndpointInfo where
  rnf RealtimeEndpointInfo' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf endpointUrl
      `Prelude.seq` Prelude.rnf peakRequestsPerSecond
