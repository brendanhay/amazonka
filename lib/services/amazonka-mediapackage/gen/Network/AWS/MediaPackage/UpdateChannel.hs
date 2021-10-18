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
-- Module      : Network.AWS.MediaPackage.UpdateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Channel.
module Network.AWS.MediaPackage.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_description,
    updateChannel_id,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_arn,
    updateChannelResponse_id,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_tags,
    updateChannelResponse_description,
    updateChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to update the Channel.
--
-- /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel to update.
    id :: Prelude.Text
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
-- 'description', 'updateChannel_description' - A short text description of the Channel.
--
-- 'id', 'updateChannel_id' - The ID of the Channel to update.
newUpdateChannel ::
  -- | 'id'
  Prelude.Text ->
  UpdateChannel
newUpdateChannel pId_ =
  UpdateChannel'
    { description = Prelude.Nothing,
      id = pId_
    }

-- | A short text description of the Channel.
updateChannel_description :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Text)
updateChannel_description = Lens.lens (\UpdateChannel' {description} -> description) (\s@UpdateChannel' {} a -> s {description = a} :: UpdateChannel)

-- | The ID of the Channel to update.
updateChannel_id :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_id = Lens.lens (\UpdateChannel' {id} -> id) (\s@UpdateChannel' {} a -> s {id = a} :: UpdateChannel)

instance Core.AWSRequest UpdateChannel where
  type
    AWSResponse UpdateChannel =
      UpdateChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Core..?> "egressAccessLogs")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "hlsIngest")
            Prelude.<*> (x Core..?> "ingressAccessLogs")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannel

instance Prelude.NFData UpdateChannel

instance Core.ToHeaders UpdateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [("description" Core..=) Prelude.<$> description]
      )

instance Core.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Prelude.mconcat ["/channels/", Core.toBS id]

instance Core.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    hlsIngest :: Prelude.Maybe HlsIngest,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'egressAccessLogs', 'updateChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'arn', 'updateChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'updateChannelResponse_id' - The ID of the Channel.
--
-- 'hlsIngest', 'updateChannelResponse_hlsIngest' - Undocumented member.
--
-- 'ingressAccessLogs', 'updateChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'updateChannelResponse_tags' - Undocumented member.
--
-- 'description', 'updateChannelResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
newUpdateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelResponse
newUpdateChannelResponse pHttpStatus_ =
  UpdateChannelResponse'
    { egressAccessLogs =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateChannelResponse_egressAccessLogs :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe EgressAccessLogs)
updateChannelResponse_egressAccessLogs = Lens.lens (\UpdateChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@UpdateChannelResponse' {} a -> s {egressAccessLogs = a} :: UpdateChannelResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
updateChannelResponse_arn :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_arn = Lens.lens (\UpdateChannelResponse' {arn} -> arn) (\s@UpdateChannelResponse' {} a -> s {arn = a} :: UpdateChannelResponse)

-- | The ID of the Channel.
updateChannelResponse_id :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_id = Lens.lens (\UpdateChannelResponse' {id} -> id) (\s@UpdateChannelResponse' {} a -> s {id = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_hlsIngest :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe HlsIngest)
updateChannelResponse_hlsIngest = Lens.lens (\UpdateChannelResponse' {hlsIngest} -> hlsIngest) (\s@UpdateChannelResponse' {} a -> s {hlsIngest = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_ingressAccessLogs :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe IngressAccessLogs)
updateChannelResponse_ingressAccessLogs = Lens.lens (\UpdateChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@UpdateChannelResponse' {} a -> s {ingressAccessLogs = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_tags :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateChannelResponse_tags = Lens.lens (\UpdateChannelResponse' {tags} -> tags) (\s@UpdateChannelResponse' {} a -> s {tags = a} :: UpdateChannelResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
updateChannelResponse_description :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_description = Lens.lens (\UpdateChannelResponse' {description} -> description) (\s@UpdateChannelResponse' {} a -> s {description = a} :: UpdateChannelResponse)

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Prelude.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

instance Prelude.NFData UpdateChannelResponse
