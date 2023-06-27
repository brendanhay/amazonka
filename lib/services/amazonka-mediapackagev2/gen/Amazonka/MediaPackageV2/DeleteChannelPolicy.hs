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
-- Module      : Amazonka.MediaPackageV2.DeleteChannelPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a channel policy.
module Amazonka.MediaPackageV2.DeleteChannelPolicy
  ( -- * Creating a Request
    DeleteChannelPolicy (..),
    newDeleteChannelPolicy,

    -- * Request Lenses
    deleteChannelPolicy_channelGroupName,
    deleteChannelPolicy_channelName,

    -- * Destructuring the Response
    DeleteChannelPolicyResponse (..),
    newDeleteChannelPolicyResponse,

    -- * Response Lenses
    deleteChannelPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelPolicy' smart constructor.
data DeleteChannelPolicy = DeleteChannelPolicy'
  { -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'deleteChannelPolicy_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'deleteChannelPolicy_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
newDeleteChannelPolicy ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  DeleteChannelPolicy
newDeleteChannelPolicy
  pChannelGroupName_
  pChannelName_ =
    DeleteChannelPolicy'
      { channelGroupName =
          pChannelGroupName_,
        channelName = pChannelName_
      }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
deleteChannelPolicy_channelGroupName :: Lens.Lens' DeleteChannelPolicy Prelude.Text
deleteChannelPolicy_channelGroupName = Lens.lens (\DeleteChannelPolicy' {channelGroupName} -> channelGroupName) (\s@DeleteChannelPolicy' {} a -> s {channelGroupName = a} :: DeleteChannelPolicy)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
deleteChannelPolicy_channelName :: Lens.Lens' DeleteChannelPolicy Prelude.Text
deleteChannelPolicy_channelName = Lens.lens (\DeleteChannelPolicy' {channelName} -> channelName) (\s@DeleteChannelPolicy' {} a -> s {channelName = a} :: DeleteChannelPolicy)

instance Core.AWSRequest DeleteChannelPolicy where
  type
    AWSResponse DeleteChannelPolicy =
      DeleteChannelPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChannelPolicy where
  hashWithSalt _salt DeleteChannelPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData DeleteChannelPolicy where
  rnf DeleteChannelPolicy' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders DeleteChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteChannelPolicy where
  toPath DeleteChannelPolicy' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/policy"
      ]

instance Data.ToQuery DeleteChannelPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelPolicyResponse' smart constructor.
data DeleteChannelPolicyResponse = DeleteChannelPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteChannelPolicyResponse_httpStatus' - The response's http status code.
newDeleteChannelPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChannelPolicyResponse
newDeleteChannelPolicyResponse pHttpStatus_ =
  DeleteChannelPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteChannelPolicyResponse_httpStatus :: Lens.Lens' DeleteChannelPolicyResponse Prelude.Int
deleteChannelPolicyResponse_httpStatus = Lens.lens (\DeleteChannelPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteChannelPolicyResponse' {} a -> s {httpStatus = a} :: DeleteChannelPolicyResponse)

instance Prelude.NFData DeleteChannelPolicyResponse where
  rnf DeleteChannelPolicyResponse' {..} =
    Prelude.rnf httpStatus
