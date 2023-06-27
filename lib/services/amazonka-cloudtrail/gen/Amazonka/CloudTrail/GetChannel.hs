{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrail.GetChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific channel.
module Amazonka.CloudTrail.GetChannel
  ( -- * Creating a Request
    GetChannel (..),
    newGetChannel,

    -- * Request Lenses
    getChannel_channel,

    -- * Destructuring the Response
    GetChannelResponse (..),
    newGetChannelResponse,

    -- * Response Lenses
    getChannelResponse_channelArn,
    getChannelResponse_destinations,
    getChannelResponse_ingestionStatus,
    getChannelResponse_name,
    getChannelResponse_source,
    getChannelResponse_sourceConfig,
    getChannelResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannel' smart constructor.
data GetChannel = GetChannel'
  { -- | The ARN or @UUID@ of a channel.
    channel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'getChannel_channel' - The ARN or @UUID@ of a channel.
newGetChannel ::
  -- | 'channel'
  Prelude.Text ->
  GetChannel
newGetChannel pChannel_ =
  GetChannel' {channel = pChannel_}

-- | The ARN or @UUID@ of a channel.
getChannel_channel :: Lens.Lens' GetChannel Prelude.Text
getChannel_channel = Lens.lens (\GetChannel' {channel} -> channel) (\s@GetChannel' {} a -> s {channel = a} :: GetChannel)

instance Core.AWSRequest GetChannel where
  type AWSResponse GetChannel = GetChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Destinations")
            Prelude.<*> (x Data..?> "IngestionStatus")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (x Data..?> "SourceConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannel where
  hashWithSalt _salt GetChannel' {..} =
    _salt `Prelude.hashWithSalt` channel

instance Prelude.NFData GetChannel where
  rnf GetChannel' {..} = Prelude.rnf channel

instance Data.ToHeaders GetChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetChannel where
  toJSON GetChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Channel" Data..= channel)]
      )

instance Data.ToPath GetChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery GetChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelResponse' smart constructor.
data GetChannelResponse = GetChannelResponse'
  { -- | The ARN of an channel returned by a @GetChannel@ request.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The destinations for the channel. For channels created for integrations,
    -- the destinations are the event data stores that log events arriving
    -- through the channel. For service-linked channels, the destination is the
    -- Amazon Web Services service that created the service-linked channel to
    -- receive events.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Destination),
    -- | A table showing information about the most recent successful and failed
    -- attempts to ingest events.
    ingestionStatus :: Prelude.Maybe IngestionStatus,
    -- | The name of the CloudTrail channel. For service-linked channels, the
    -- name is @aws-service-channel\/service-name\/custom-suffix@ where
    -- @service-name@ represents the name of the Amazon Web Services service
    -- that created the channel and @custom-suffix@ represents the suffix
    -- generated by the Amazon Web Services service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source for the CloudTrail channel.
    source :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the advanced event selectors configured for
    -- the channel, and whether the channel applies to all Regions or a single
    -- Region.
    sourceConfig :: Prelude.Maybe SourceConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'getChannelResponse_channelArn' - The ARN of an channel returned by a @GetChannel@ request.
--
-- 'destinations', 'getChannelResponse_destinations' - The destinations for the channel. For channels created for integrations,
-- the destinations are the event data stores that log events arriving
-- through the channel. For service-linked channels, the destination is the
-- Amazon Web Services service that created the service-linked channel to
-- receive events.
--
-- 'ingestionStatus', 'getChannelResponse_ingestionStatus' - A table showing information about the most recent successful and failed
-- attempts to ingest events.
--
-- 'name', 'getChannelResponse_name' - The name of the CloudTrail channel. For service-linked channels, the
-- name is @aws-service-channel\/service-name\/custom-suffix@ where
-- @service-name@ represents the name of the Amazon Web Services service
-- that created the channel and @custom-suffix@ represents the suffix
-- generated by the Amazon Web Services service.
--
-- 'source', 'getChannelResponse_source' - The source for the CloudTrail channel.
--
-- 'sourceConfig', 'getChannelResponse_sourceConfig' - Provides information about the advanced event selectors configured for
-- the channel, and whether the channel applies to all Regions or a single
-- Region.
--
-- 'httpStatus', 'getChannelResponse_httpStatus' - The response's http status code.
newGetChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelResponse
newGetChannelResponse pHttpStatus_ =
  GetChannelResponse'
    { channelArn = Prelude.Nothing,
      destinations = Prelude.Nothing,
      ingestionStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of an channel returned by a @GetChannel@ request.
getChannelResponse_channelArn :: Lens.Lens' GetChannelResponse (Prelude.Maybe Prelude.Text)
getChannelResponse_channelArn = Lens.lens (\GetChannelResponse' {channelArn} -> channelArn) (\s@GetChannelResponse' {} a -> s {channelArn = a} :: GetChannelResponse)

-- | The destinations for the channel. For channels created for integrations,
-- the destinations are the event data stores that log events arriving
-- through the channel. For service-linked channels, the destination is the
-- Amazon Web Services service that created the service-linked channel to
-- receive events.
getChannelResponse_destinations :: Lens.Lens' GetChannelResponse (Prelude.Maybe (Prelude.NonEmpty Destination))
getChannelResponse_destinations = Lens.lens (\GetChannelResponse' {destinations} -> destinations) (\s@GetChannelResponse' {} a -> s {destinations = a} :: GetChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | A table showing information about the most recent successful and failed
-- attempts to ingest events.
getChannelResponse_ingestionStatus :: Lens.Lens' GetChannelResponse (Prelude.Maybe IngestionStatus)
getChannelResponse_ingestionStatus = Lens.lens (\GetChannelResponse' {ingestionStatus} -> ingestionStatus) (\s@GetChannelResponse' {} a -> s {ingestionStatus = a} :: GetChannelResponse)

-- | The name of the CloudTrail channel. For service-linked channels, the
-- name is @aws-service-channel\/service-name\/custom-suffix@ where
-- @service-name@ represents the name of the Amazon Web Services service
-- that created the channel and @custom-suffix@ represents the suffix
-- generated by the Amazon Web Services service.
getChannelResponse_name :: Lens.Lens' GetChannelResponse (Prelude.Maybe Prelude.Text)
getChannelResponse_name = Lens.lens (\GetChannelResponse' {name} -> name) (\s@GetChannelResponse' {} a -> s {name = a} :: GetChannelResponse)

-- | The source for the CloudTrail channel.
getChannelResponse_source :: Lens.Lens' GetChannelResponse (Prelude.Maybe Prelude.Text)
getChannelResponse_source = Lens.lens (\GetChannelResponse' {source} -> source) (\s@GetChannelResponse' {} a -> s {source = a} :: GetChannelResponse)

-- | Provides information about the advanced event selectors configured for
-- the channel, and whether the channel applies to all Regions or a single
-- Region.
getChannelResponse_sourceConfig :: Lens.Lens' GetChannelResponse (Prelude.Maybe SourceConfig)
getChannelResponse_sourceConfig = Lens.lens (\GetChannelResponse' {sourceConfig} -> sourceConfig) (\s@GetChannelResponse' {} a -> s {sourceConfig = a} :: GetChannelResponse)

-- | The response's http status code.
getChannelResponse_httpStatus :: Lens.Lens' GetChannelResponse Prelude.Int
getChannelResponse_httpStatus = Lens.lens (\GetChannelResponse' {httpStatus} -> httpStatus) (\s@GetChannelResponse' {} a -> s {httpStatus = a} :: GetChannelResponse)

instance Prelude.NFData GetChannelResponse where
  rnf GetChannelResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf ingestionStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf sourceConfig
      `Prelude.seq` Prelude.rnf httpStatus
