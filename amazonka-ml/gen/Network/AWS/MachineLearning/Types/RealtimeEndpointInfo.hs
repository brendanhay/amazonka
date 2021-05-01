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
-- Module      : Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RealtimeEndpointInfo where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
import qualified Network.AWS.Prelude as Prelude

-- | Describes the real-time endpoint information for an @MLModel@.
--
-- /See:/ 'newRealtimeEndpointInfo' smart constructor.
data RealtimeEndpointInfo = RealtimeEndpointInfo'
  { -- | The time that the request to create the real-time endpoint for the
    -- @MLModel@ was received. The time is expressed in epoch time.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The maximum processing rate for the real-time endpoint for @MLModel@,
    -- measured in incoming requests per second.
    peakRequestsPerSecond :: Prelude.Maybe Prelude.Int,
    -- | The current status of the real-time endpoint for the @MLModel@. This
    -- element can have one of the following values:
    --
    -- -   @NONE@ - Endpoint does not exist or was previously deleted.
    -- -   @READY@ - Endpoint is ready to be used for real-time predictions.
    -- -   @UPDATING@ - Updating\/creating the endpoint.
    endpointStatus :: Prelude.Maybe RealtimeEndpointStatus,
    -- | The URI that specifies where to send real-time prediction requests for
    -- the @MLModel@.
    --
    -- Note
    --
    -- The application must wait until the real-time endpoint is ready before
    -- using this URI.
    endpointUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'peakRequestsPerSecond', 'realtimeEndpointInfo_peakRequestsPerSecond' - The maximum processing rate for the real-time endpoint for @MLModel@,
-- measured in incoming requests per second.
--
-- 'endpointStatus', 'realtimeEndpointInfo_endpointStatus' - The current status of the real-time endpoint for the @MLModel@. This
-- element can have one of the following values:
--
-- -   @NONE@ - Endpoint does not exist or was previously deleted.
-- -   @READY@ - Endpoint is ready to be used for real-time predictions.
-- -   @UPDATING@ - Updating\/creating the endpoint.
--
-- 'endpointUrl', 'realtimeEndpointInfo_endpointUrl' - The URI that specifies where to send real-time prediction requests for
-- the @MLModel@.
--
-- Note
--
-- The application must wait until the real-time endpoint is ready before
-- using this URI.
newRealtimeEndpointInfo ::
  RealtimeEndpointInfo
newRealtimeEndpointInfo =
  RealtimeEndpointInfo'
    { createdAt = Prelude.Nothing,
      peakRequestsPerSecond = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      endpointUrl = Prelude.Nothing
    }

-- | The time that the request to create the real-time endpoint for the
-- @MLModel@ was received. The time is expressed in epoch time.
realtimeEndpointInfo_createdAt :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe Prelude.UTCTime)
realtimeEndpointInfo_createdAt = Lens.lens (\RealtimeEndpointInfo' {createdAt} -> createdAt) (\s@RealtimeEndpointInfo' {} a -> s {createdAt = a} :: RealtimeEndpointInfo) Prelude.. Lens.mapping Prelude._Time

-- | The maximum processing rate for the real-time endpoint for @MLModel@,
-- measured in incoming requests per second.
realtimeEndpointInfo_peakRequestsPerSecond :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe Prelude.Int)
realtimeEndpointInfo_peakRequestsPerSecond = Lens.lens (\RealtimeEndpointInfo' {peakRequestsPerSecond} -> peakRequestsPerSecond) (\s@RealtimeEndpointInfo' {} a -> s {peakRequestsPerSecond = a} :: RealtimeEndpointInfo)

-- | The current status of the real-time endpoint for the @MLModel@. This
-- element can have one of the following values:
--
-- -   @NONE@ - Endpoint does not exist or was previously deleted.
-- -   @READY@ - Endpoint is ready to be used for real-time predictions.
-- -   @UPDATING@ - Updating\/creating the endpoint.
realtimeEndpointInfo_endpointStatus :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe RealtimeEndpointStatus)
realtimeEndpointInfo_endpointStatus = Lens.lens (\RealtimeEndpointInfo' {endpointStatus} -> endpointStatus) (\s@RealtimeEndpointInfo' {} a -> s {endpointStatus = a} :: RealtimeEndpointInfo)

-- | The URI that specifies where to send real-time prediction requests for
-- the @MLModel@.
--
-- Note
--
-- The application must wait until the real-time endpoint is ready before
-- using this URI.
realtimeEndpointInfo_endpointUrl :: Lens.Lens' RealtimeEndpointInfo (Prelude.Maybe Prelude.Text)
realtimeEndpointInfo_endpointUrl = Lens.lens (\RealtimeEndpointInfo' {endpointUrl} -> endpointUrl) (\s@RealtimeEndpointInfo' {} a -> s {endpointUrl = a} :: RealtimeEndpointInfo)

instance Prelude.FromJSON RealtimeEndpointInfo where
  parseJSON =
    Prelude.withObject
      "RealtimeEndpointInfo"
      ( \x ->
          RealtimeEndpointInfo'
            Prelude.<$> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "PeakRequestsPerSecond")
            Prelude.<*> (x Prelude..:? "EndpointStatus")
            Prelude.<*> (x Prelude..:? "EndpointUrl")
      )

instance Prelude.Hashable RealtimeEndpointInfo

instance Prelude.NFData RealtimeEndpointInfo
