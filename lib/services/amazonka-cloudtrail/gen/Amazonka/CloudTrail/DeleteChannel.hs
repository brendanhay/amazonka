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
-- Module      : Amazonka.CloudTrail.DeleteChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a channel.
module Amazonka.CloudTrail.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_channel,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,

    -- * Response Lenses
    deleteChannelResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | The ARN or the @UUID@ value of the channel that you want to delete.
    channel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'deleteChannel_channel' - The ARN or the @UUID@ value of the channel that you want to delete.
newDeleteChannel ::
  -- | 'channel'
  Prelude.Text ->
  DeleteChannel
newDeleteChannel pChannel_ =
  DeleteChannel' {channel = pChannel_}

-- | The ARN or the @UUID@ value of the channel that you want to delete.
deleteChannel_channel :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_channel = Lens.lens (\DeleteChannel' {channel} -> channel) (\s@DeleteChannel' {} a -> s {channel = a} :: DeleteChannel)

instance Core.AWSRequest DeleteChannel where
  type
    AWSResponse DeleteChannel =
      DeleteChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChannel where
  hashWithSalt _salt DeleteChannel' {..} =
    _salt `Prelude.hashWithSalt` channel

instance Prelude.NFData DeleteChannel where
  rnf DeleteChannel' {..} = Prelude.rnf channel

instance Data.ToHeaders DeleteChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeleteChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteChannel where
  toJSON DeleteChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Channel" Data..= channel)]
      )

instance Data.ToPath DeleteChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteChannelResponse_httpStatus' - The response's http status code.
newDeleteChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChannelResponse
newDeleteChannelResponse pHttpStatus_ =
  DeleteChannelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteChannelResponse_httpStatus :: Lens.Lens' DeleteChannelResponse Prelude.Int
deleteChannelResponse_httpStatus = Lens.lens (\DeleteChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteChannelResponse' {} a -> s {httpStatus = a} :: DeleteChannelResponse)

instance Prelude.NFData DeleteChannelResponse where
  rnf DeleteChannelResponse' {..} =
    Prelude.rnf httpStatus
