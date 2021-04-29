{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.CreateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Channel.
module Network.AWS.MediaPackage.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_tags,
    createChannel_description,
    createChannel_id,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_egressAccessLogs,
    createChannelResponse_hlsIngest,
    createChannelResponse_arn,
    createChannelResponse_id,
    createChannelResponse_ingressAccessLogs,
    createChannelResponse_tags,
    createChannelResponse_description,
    createChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A new Channel configuration.
--
-- /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel. The ID must be unique within the region and it
    -- cannot be changed after a Channel is created.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'description', 'createChannel_description' - A short text description of the Channel.
--
-- 'id', 'createChannel_id' - The ID of the Channel. The ID must be unique within the region and it
-- cannot be changed after a Channel is created.
newCreateChannel ::
  -- | 'id'
  Prelude.Text ->
  CreateChannel
newCreateChannel pId_ =
  CreateChannel'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Prelude._Coerce

-- | A short text description of the Channel.
createChannel_description :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_description = Lens.lens (\CreateChannel' {description} -> description) (\s@CreateChannel' {} a -> s {description = a} :: CreateChannel)

-- | The ID of the Channel. The ID must be unique within the region and it
-- cannot be changed after a Channel is created.
createChannel_id :: Lens.Lens' CreateChannel Prelude.Text
createChannel_id = Lens.lens (\CreateChannel' {id} -> id) (\s@CreateChannel' {} a -> s {id = a} :: CreateChannel)

instance Prelude.AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Prelude..?> "egressAccessLogs")
            Prelude.<*> (x Prelude..?> "hlsIngest")
            Prelude.<*> (x Prelude..?> "arn")
            Prelude.<*> (x Prelude..?> "id")
            Prelude.<*> (x Prelude..?> "ingressAccessLogs")
            Prelude.<*> (x Prelude..?> "tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel

instance Prelude.NFData CreateChannel

instance Prelude.ToHeaders CreateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("id" Prelude..= id)
          ]
      )

instance Prelude.ToPath CreateChannel where
  toPath = Prelude.const "/channels"

instance Prelude.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'createChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'createChannelResponse_hlsIngest' - Undocumented member.
--
-- 'arn', 'createChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'createChannelResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'createChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'createChannelResponse_tags' - Undocumented member.
--
-- 'description', 'createChannelResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { egressAccessLogs =
        Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createChannelResponse_egressAccessLogs :: Lens.Lens' CreateChannelResponse (Prelude.Maybe EgressAccessLogs)
createChannelResponse_egressAccessLogs = Lens.lens (\CreateChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@CreateChannelResponse' {} a -> s {egressAccessLogs = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_hlsIngest :: Lens.Lens' CreateChannelResponse (Prelude.Maybe HlsIngest)
createChannelResponse_hlsIngest = Lens.lens (\CreateChannelResponse' {hlsIngest} -> hlsIngest) (\s@CreateChannelResponse' {} a -> s {hlsIngest = a} :: CreateChannelResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
createChannelResponse_arn :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_arn = Lens.lens (\CreateChannelResponse' {arn} -> arn) (\s@CreateChannelResponse' {} a -> s {arn = a} :: CreateChannelResponse)

-- | The ID of the Channel.
createChannelResponse_id :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_id = Lens.lens (\CreateChannelResponse' {id} -> id) (\s@CreateChannelResponse' {} a -> s {id = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_ingressAccessLogs :: Lens.Lens' CreateChannelResponse (Prelude.Maybe IngressAccessLogs)
createChannelResponse_ingressAccessLogs = Lens.lens (\CreateChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@CreateChannelResponse' {} a -> s {ingressAccessLogs = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_tags :: Lens.Lens' CreateChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannelResponse_tags = Lens.lens (\CreateChannelResponse' {tags} -> tags) (\s@CreateChannelResponse' {} a -> s {tags = a} :: CreateChannelResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A short text description of the Channel.
createChannelResponse_description :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_description = Lens.lens (\CreateChannelResponse' {description} -> description) (\s@CreateChannelResponse' {} a -> s {description = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse
