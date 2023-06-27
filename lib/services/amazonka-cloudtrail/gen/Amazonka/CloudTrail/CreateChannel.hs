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
-- Module      : Amazonka.CloudTrail.CreateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel for CloudTrail to ingest events from a partner or
-- external source. After you create a channel, a CloudTrail Lake event
-- data store can log events from the partner or source that you specify.
module Amazonka.CloudTrail.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_tags,
    createChannel_name,
    createChannel_source,
    createChannel_destinations,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channelArn,
    createChannelResponse_destinations,
    createChannelResponse_name,
    createChannelResponse_source,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { tags :: Prelude.Maybe [Tag],
    -- | The name of the channel.
    name :: Prelude.Text,
    -- | The name of the partner or external event source. You cannot change this
    -- name after you create the channel. A maximum of one channel is allowed
    -- per source.
    --
    -- A source can be either @Custom@ for all valid non-Amazon Web Services
    -- events, or the name of a partner event source. For information about the
    -- source names for available partners, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/query-event-data-store-integration.html#cloudtrail-lake-partner-information Additional information about integration partners>
    -- in the CloudTrail User Guide.
    source :: Prelude.Text,
    -- | One or more event data stores to which events arriving through a channel
    -- will be logged.
    destinations :: Prelude.NonEmpty Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createChannel_tags' - Undocumented member.
--
-- 'name', 'createChannel_name' - The name of the channel.
--
-- 'source', 'createChannel_source' - The name of the partner or external event source. You cannot change this
-- name after you create the channel. A maximum of one channel is allowed
-- per source.
--
-- A source can be either @Custom@ for all valid non-Amazon Web Services
-- events, or the name of a partner event source. For information about the
-- source names for available partners, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/query-event-data-store-integration.html#cloudtrail-lake-partner-information Additional information about integration partners>
-- in the CloudTrail User Guide.
--
-- 'destinations', 'createChannel_destinations' - One or more event data stores to which events arriving through a channel
-- will be logged.
newCreateChannel ::
  -- | 'name'
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  -- | 'destinations'
  Prelude.NonEmpty Destination ->
  CreateChannel
newCreateChannel pName_ pSource_ pDestinations_ =
  CreateChannel'
    { tags = Prelude.Nothing,
      name = pName_,
      source = pSource_,
      destinations = Lens.coerced Lens.# pDestinations_
    }

-- | Undocumented member.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe [Tag])
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel.
createChannel_name :: Lens.Lens' CreateChannel Prelude.Text
createChannel_name = Lens.lens (\CreateChannel' {name} -> name) (\s@CreateChannel' {} a -> s {name = a} :: CreateChannel)

-- | The name of the partner or external event source. You cannot change this
-- name after you create the channel. A maximum of one channel is allowed
-- per source.
--
-- A source can be either @Custom@ for all valid non-Amazon Web Services
-- events, or the name of a partner event source. For information about the
-- source names for available partners, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/query-event-data-store-integration.html#cloudtrail-lake-partner-information Additional information about integration partners>
-- in the CloudTrail User Guide.
createChannel_source :: Lens.Lens' CreateChannel Prelude.Text
createChannel_source = Lens.lens (\CreateChannel' {source} -> source) (\s@CreateChannel' {} a -> s {source = a} :: CreateChannel)

-- | One or more event data stores to which events arriving through a channel
-- will be logged.
createChannel_destinations :: Lens.Lens' CreateChannel (Prelude.NonEmpty Destination)
createChannel_destinations = Lens.lens (\CreateChannel' {destinations} -> destinations) (\s@CreateChannel' {} a -> s {destinations = a} :: CreateChannel) Prelude.. Lens.coerced

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Destinations")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` destinations

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf destinations

instance Data.ToHeaders CreateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CreateChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("Destinations" Data..= destinations)
          ]
      )

instance Data.ToPath CreateChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The Amazon Resource Name (ARN) of the new channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The event data stores that log the events arriving through the channel.
    destinations :: Prelude.Maybe (Prelude.NonEmpty Destination),
    -- | The name of the new channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | The partner or external event source name.
    source :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelResponse_channelArn' - The Amazon Resource Name (ARN) of the new channel.
--
-- 'destinations', 'createChannelResponse_destinations' - The event data stores that log the events arriving through the channel.
--
-- 'name', 'createChannelResponse_name' - The name of the new channel.
--
-- 'source', 'createChannelResponse_source' - The partner or external event source name.
--
-- 'tags', 'createChannelResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { channelArn =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      source = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new channel.
createChannelResponse_channelArn :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelArn = Lens.lens (\CreateChannelResponse' {channelArn} -> channelArn) (\s@CreateChannelResponse' {} a -> s {channelArn = a} :: CreateChannelResponse)

-- | The event data stores that log the events arriving through the channel.
createChannelResponse_destinations :: Lens.Lens' CreateChannelResponse (Prelude.Maybe (Prelude.NonEmpty Destination))
createChannelResponse_destinations = Lens.lens (\CreateChannelResponse' {destinations} -> destinations) (\s@CreateChannelResponse' {} a -> s {destinations = a} :: CreateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new channel.
createChannelResponse_name :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_name = Lens.lens (\CreateChannelResponse' {name} -> name) (\s@CreateChannelResponse' {} a -> s {name = a} :: CreateChannelResponse)

-- | The partner or external event source name.
createChannelResponse_source :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_source = Lens.lens (\CreateChannelResponse' {source} -> source) (\s@CreateChannelResponse' {} a -> s {source = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_tags :: Lens.Lens' CreateChannelResponse (Prelude.Maybe [Tag])
createChannelResponse_tags = Lens.lens (\CreateChannelResponse' {tags} -> tags) (\s@CreateChannelResponse' {} a -> s {tags = a} :: CreateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
