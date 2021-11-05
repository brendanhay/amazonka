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
-- Module      : Network.AWS.MediaTailor.DeleteChannelPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a channel\'s IAM policy.
module Network.AWS.MediaTailor.DeleteChannelPolicy
  ( -- * Creating a Request
    DeleteChannelPolicy (..),
    newDeleteChannelPolicy,

    -- * Request Lenses
    deleteChannelPolicy_channelName,

    -- * Destructuring the Response
    DeleteChannelPolicyResponse (..),
    newDeleteChannelPolicyResponse,

    -- * Response Lenses
    deleteChannelPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteChannelPolicy' smart constructor.
data DeleteChannelPolicy = DeleteChannelPolicy'
  { -- | The identifier for the channel you are working on.
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
-- 'channelName', 'deleteChannelPolicy_channelName' - The identifier for the channel you are working on.
newDeleteChannelPolicy ::
  -- | 'channelName'
  Prelude.Text ->
  DeleteChannelPolicy
newDeleteChannelPolicy pChannelName_ =
  DeleteChannelPolicy' {channelName = pChannelName_}

-- | The identifier for the channel you are working on.
deleteChannelPolicy_channelName :: Lens.Lens' DeleteChannelPolicy Prelude.Text
deleteChannelPolicy_channelName = Lens.lens (\DeleteChannelPolicy' {channelName} -> channelName) (\s@DeleteChannelPolicy' {} a -> s {channelName = a} :: DeleteChannelPolicy)

instance Core.AWSRequest DeleteChannelPolicy where
  type
    AWSResponse DeleteChannelPolicy =
      DeleteChannelPolicyResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChannelPolicy

instance Prelude.NFData DeleteChannelPolicy

instance Core.ToHeaders DeleteChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteChannelPolicy where
  toPath DeleteChannelPolicy' {..} =
    Prelude.mconcat
      ["/channel/", Core.toBS channelName, "/policy"]

instance Core.ToQuery DeleteChannelPolicy where
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

instance Prelude.NFData DeleteChannelPolicyResponse
