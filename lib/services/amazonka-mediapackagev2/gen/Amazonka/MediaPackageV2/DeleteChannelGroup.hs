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
-- Module      : Amazonka.MediaPackageV2.DeleteChannelGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a channel group. You must delete the channel group\'s channels
-- and origin endpoints before you can delete the channel group. If you
-- delete a channel group, you\'ll lose access to the egress domain and
-- will have to create a new channel group to replace it.
module Amazonka.MediaPackageV2.DeleteChannelGroup
  ( -- * Creating a Request
    DeleteChannelGroup (..),
    newDeleteChannelGroup,

    -- * Request Lenses
    deleteChannelGroup_channelGroupName,

    -- * Destructuring the Response
    DeleteChannelGroupResponse (..),
    newDeleteChannelGroupResponse,

    -- * Response Lenses
    deleteChannelGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelGroup' smart constructor.
data DeleteChannelGroup = DeleteChannelGroup'
  { -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'deleteChannelGroup_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
newDeleteChannelGroup ::
  -- | 'channelGroupName'
  Prelude.Text ->
  DeleteChannelGroup
newDeleteChannelGroup pChannelGroupName_ =
  DeleteChannelGroup'
    { channelGroupName =
        pChannelGroupName_
    }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
deleteChannelGroup_channelGroupName :: Lens.Lens' DeleteChannelGroup Prelude.Text
deleteChannelGroup_channelGroupName = Lens.lens (\DeleteChannelGroup' {channelGroupName} -> channelGroupName) (\s@DeleteChannelGroup' {} a -> s {channelGroupName = a} :: DeleteChannelGroup)

instance Core.AWSRequest DeleteChannelGroup where
  type
    AWSResponse DeleteChannelGroup =
      DeleteChannelGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChannelGroup where
  hashWithSalt _salt DeleteChannelGroup' {..} =
    _salt `Prelude.hashWithSalt` channelGroupName

instance Prelude.NFData DeleteChannelGroup where
  rnf DeleteChannelGroup' {..} =
    Prelude.rnf channelGroupName

instance Data.ToHeaders DeleteChannelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteChannelGroup where
  toPath DeleteChannelGroup' {..} =
    Prelude.mconcat
      ["/channelGroup/", Data.toBS channelGroupName]

instance Data.ToQuery DeleteChannelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelGroupResponse' smart constructor.
data DeleteChannelGroupResponse = DeleteChannelGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteChannelGroupResponse_httpStatus' - The response's http status code.
newDeleteChannelGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChannelGroupResponse
newDeleteChannelGroupResponse pHttpStatus_ =
  DeleteChannelGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteChannelGroupResponse_httpStatus :: Lens.Lens' DeleteChannelGroupResponse Prelude.Int
deleteChannelGroupResponse_httpStatus = Lens.lens (\DeleteChannelGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteChannelGroupResponse' {} a -> s {httpStatus = a} :: DeleteChannelGroupResponse)

instance Prelude.NFData DeleteChannelGroupResponse where
  rnf DeleteChannelGroupResponse' {..} =
    Prelude.rnf httpStatus
