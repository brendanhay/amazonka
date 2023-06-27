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
-- Module      : Amazonka.CloudTrail.UpdateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel specified by a required channel ARN or UUID.
module Amazonka.CloudTrail.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_destinations,
    updateChannel_name,
    updateChannel_channel,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_channelArn,
    updateChannelResponse_destinations,
    updateChannelResponse_name,
    updateChannelResponse_source,
    updateChannelResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | The ARNs of event data stores that you want to log events arriving
    -- through the channel.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Destination),
    -- | Changes the name of the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN or ID (the ARN suffix) of the channel that you want to update.
    channel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'updateChannel_destinations' - The ARNs of event data stores that you want to log events arriving
-- through the channel.
--
-- 'name', 'updateChannel_name' - Changes the name of the channel.
--
-- 'channel', 'updateChannel_channel' - The ARN or ID (the ARN suffix) of the channel that you want to update.
newUpdateChannel ::
  -- | 'channel'
  Prelude.Text ->
  UpdateChannel
newUpdateChannel pChannel_ =
  UpdateChannel'
    { destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      channel = pChannel_
    }

-- | The ARNs of event data stores that you want to log events arriving
-- through the channel.
updateChannel_destinations :: Lens.Lens' UpdateChannel (Prelude.Maybe (Prelude.NonEmpty Destination))
updateChannel_destinations = Lens.lens (\UpdateChannel' {destinations} -> destinations) (\s@UpdateChannel' {} a -> s {destinations = a} :: UpdateChannel) Prelude.. Lens.mapping Lens.coerced

-- | Changes the name of the channel.
updateChannel_name :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Text)
updateChannel_name = Lens.lens (\UpdateChannel' {name} -> name) (\s@UpdateChannel' {} a -> s {name = a} :: UpdateChannel)

-- | The ARN or ID (the ARN suffix) of the channel that you want to update.
updateChannel_channel :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_channel = Lens.lens (\UpdateChannel' {channel} -> channel) (\s@UpdateChannel' {} a -> s {channel = a} :: UpdateChannel)

instance Core.AWSRequest UpdateChannel where
  type
    AWSResponse UpdateChannel =
      UpdateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Destinations")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannel where
  hashWithSalt _salt UpdateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` channel

instance Prelude.NFData UpdateChannel where
  rnf UpdateChannel' {..} =
    Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf channel

instance Data.ToHeaders UpdateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Destinations" Data..=) Prelude.<$> destinations,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("Channel" Data..= channel)
          ]
      )

instance Data.ToPath UpdateChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { -- | The ARN of the channel that was updated.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The event data stores that log events arriving through the channel.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Destination),
    -- | The name of the channel that was updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | The event source of the channel that was updated.
    source :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'updateChannelResponse_channelArn' - The ARN of the channel that was updated.
--
-- 'destinations', 'updateChannelResponse_destinations' - The event data stores that log events arriving through the channel.
--
-- 'name', 'updateChannelResponse_name' - The name of the channel that was updated.
--
-- 'source', 'updateChannelResponse_source' - The event source of the channel that was updated.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
newUpdateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelResponse
newUpdateChannelResponse pHttpStatus_ =
  UpdateChannelResponse'
    { channelArn =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      source = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel that was updated.
updateChannelResponse_channelArn :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_channelArn = Lens.lens (\UpdateChannelResponse' {channelArn} -> channelArn) (\s@UpdateChannelResponse' {} a -> s {channelArn = a} :: UpdateChannelResponse)

-- | The event data stores that log events arriving through the channel.
updateChannelResponse_destinations :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe (Prelude.NonEmpty Destination))
updateChannelResponse_destinations = Lens.lens (\UpdateChannelResponse' {destinations} -> destinations) (\s@UpdateChannelResponse' {} a -> s {destinations = a} :: UpdateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel that was updated.
updateChannelResponse_name :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_name = Lens.lens (\UpdateChannelResponse' {name} -> name) (\s@UpdateChannelResponse' {} a -> s {name = a} :: UpdateChannelResponse)

-- | The event source of the channel that was updated.
updateChannelResponse_source :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_source = Lens.lens (\UpdateChannelResponse' {source} -> source) (\s@UpdateChannelResponse' {} a -> s {source = a} :: UpdateChannelResponse)

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Prelude.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

instance Prelude.NFData UpdateChannelResponse where
  rnf UpdateChannelResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf httpStatus
