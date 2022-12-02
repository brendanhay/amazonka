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
-- Module      : Amazonka.MediaTailor.DeleteChannelPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The channel policy to delete.
module Amazonka.MediaTailor.DeleteChannelPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannelPolicy' smart constructor.
data DeleteChannelPolicy = DeleteChannelPolicy'
  { -- | The name of the channel associated with this channel policy.
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
-- 'channelName', 'deleteChannelPolicy_channelName' - The name of the channel associated with this channel policy.
newDeleteChannelPolicy ::
  -- | 'channelName'
  Prelude.Text ->
  DeleteChannelPolicy
newDeleteChannelPolicy pChannelName_ =
  DeleteChannelPolicy' {channelName = pChannelName_}

-- | The name of the channel associated with this channel policy.
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
    _salt `Prelude.hashWithSalt` channelName

instance Prelude.NFData DeleteChannelPolicy where
  rnf DeleteChannelPolicy' {..} =
    Prelude.rnf channelName

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
      ["/channel/", Data.toBS channelName, "/policy"]

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
