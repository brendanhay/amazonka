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
-- Module      : Network.AWS.MediaPackage.DeleteChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Channel.
module Network.AWS.MediaPackage.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_id,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,

    -- * Response Lenses
    deleteChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | The ID of the Channel to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteChannel_id' - The ID of the Channel to delete.
newDeleteChannel ::
  -- | 'id'
  Prelude.Text ->
  DeleteChannel
newDeleteChannel pId_ = DeleteChannel' {id = pId_}

-- | The ID of the Channel to delete.
deleteChannel_id :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_id = Lens.lens (\DeleteChannel' {id} -> id) (\s@DeleteChannel' {} a -> s {id = a} :: DeleteChannel)

instance Prelude.AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChannel

instance Prelude.NFData DeleteChannel

instance Prelude.ToHeaders DeleteChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Prelude.mconcat ["/channels/", Prelude.toBS id]

instance Prelude.ToQuery DeleteChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteChannelResponse
