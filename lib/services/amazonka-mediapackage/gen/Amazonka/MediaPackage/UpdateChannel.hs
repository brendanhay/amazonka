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
-- Module      : Amazonka.MediaPackage.UpdateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Channel.
module Amazonka.MediaPackage.UpdateChannel
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
    updateChannelResponse_arn,
    updateChannelResponse_description,
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_id,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_tags,
    updateChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "hlsIngest")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "ingressAccessLogs")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannel where
  hashWithSalt _salt UpdateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateChannel where
  rnf UpdateChannel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [("description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Prelude.mconcat ["/channels/", Data.toBS id]

instance Data.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'arn', 'updateChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'description', 'updateChannelResponse_description' - A short text description of the Channel.
--
-- 'egressAccessLogs', 'updateChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'updateChannelResponse_hlsIngest' - Undocumented member.
--
-- 'id', 'updateChannelResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'updateChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'updateChannelResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
newUpdateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelResponse
newUpdateChannelResponse pHttpStatus_ =
  UpdateChannelResponse'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      id = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the Channel.
updateChannelResponse_arn :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_arn = Lens.lens (\UpdateChannelResponse' {arn} -> arn) (\s@UpdateChannelResponse' {} a -> s {arn = a} :: UpdateChannelResponse)

-- | A short text description of the Channel.
updateChannelResponse_description :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_description = Lens.lens (\UpdateChannelResponse' {description} -> description) (\s@UpdateChannelResponse' {} a -> s {description = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_egressAccessLogs :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe EgressAccessLogs)
updateChannelResponse_egressAccessLogs = Lens.lens (\UpdateChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@UpdateChannelResponse' {} a -> s {egressAccessLogs = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_hlsIngest :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe HlsIngest)
updateChannelResponse_hlsIngest = Lens.lens (\UpdateChannelResponse' {hlsIngest} -> hlsIngest) (\s@UpdateChannelResponse' {} a -> s {hlsIngest = a} :: UpdateChannelResponse)

-- | The ID of the Channel.
updateChannelResponse_id :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_id = Lens.lens (\UpdateChannelResponse' {id} -> id) (\s@UpdateChannelResponse' {} a -> s {id = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_ingressAccessLogs :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe IngressAccessLogs)
updateChannelResponse_ingressAccessLogs = Lens.lens (\UpdateChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@UpdateChannelResponse' {} a -> s {ingressAccessLogs = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_tags :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateChannelResponse_tags = Lens.lens (\UpdateChannelResponse' {tags} -> tags) (\s@UpdateChannelResponse' {} a -> s {tags = a} :: UpdateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Prelude.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

instance Prelude.NFData UpdateChannelResponse where
  rnf UpdateChannelResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf hlsIngest
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ingressAccessLogs
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
