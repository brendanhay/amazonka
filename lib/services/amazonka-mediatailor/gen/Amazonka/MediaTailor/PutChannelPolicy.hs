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
-- Module      : Amazonka.MediaTailor.PutChannelPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM policy for the channel. IAM policies are used to control
-- access to your channel.
module Amazonka.MediaTailor.PutChannelPolicy
  ( -- * Creating a Request
    PutChannelPolicy (..),
    newPutChannelPolicy,

    -- * Request Lenses
    putChannelPolicy_channelName,
    putChannelPolicy_policy,

    -- * Destructuring the Response
    PutChannelPolicyResponse (..),
    newPutChannelPolicyResponse,

    -- * Response Lenses
    putChannelPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutChannelPolicy' smart constructor.
data PutChannelPolicy = PutChannelPolicy'
  { -- | The channel name associated with this Channel Policy.
    channelName :: Prelude.Text,
    -- | Adds an IAM role that determines the permissions of your channel.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChannelPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'putChannelPolicy_channelName' - The channel name associated with this Channel Policy.
--
-- 'policy', 'putChannelPolicy_policy' - Adds an IAM role that determines the permissions of your channel.
newPutChannelPolicy ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutChannelPolicy
newPutChannelPolicy pChannelName_ pPolicy_ =
  PutChannelPolicy'
    { channelName = pChannelName_,
      policy = pPolicy_
    }

-- | The channel name associated with this Channel Policy.
putChannelPolicy_channelName :: Lens.Lens' PutChannelPolicy Prelude.Text
putChannelPolicy_channelName = Lens.lens (\PutChannelPolicy' {channelName} -> channelName) (\s@PutChannelPolicy' {} a -> s {channelName = a} :: PutChannelPolicy)

-- | Adds an IAM role that determines the permissions of your channel.
putChannelPolicy_policy :: Lens.Lens' PutChannelPolicy Prelude.Text
putChannelPolicy_policy = Lens.lens (\PutChannelPolicy' {policy} -> policy) (\s@PutChannelPolicy' {} a -> s {policy = a} :: PutChannelPolicy)

instance Core.AWSRequest PutChannelPolicy where
  type
    AWSResponse PutChannelPolicy =
      PutChannelPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutChannelPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutChannelPolicy where
  hashWithSalt _salt PutChannelPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutChannelPolicy where
  rnf PutChannelPolicy' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutChannelPolicy where
  toJSON PutChannelPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Policy" Data..= policy)]
      )

instance Data.ToPath PutChannelPolicy where
  toPath PutChannelPolicy' {..} =
    Prelude.mconcat
      ["/channel/", Data.toBS channelName, "/policy"]

instance Data.ToQuery PutChannelPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutChannelPolicyResponse' smart constructor.
data PutChannelPolicyResponse = PutChannelPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChannelPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putChannelPolicyResponse_httpStatus' - The response's http status code.
newPutChannelPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutChannelPolicyResponse
newPutChannelPolicyResponse pHttpStatus_ =
  PutChannelPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putChannelPolicyResponse_httpStatus :: Lens.Lens' PutChannelPolicyResponse Prelude.Int
putChannelPolicyResponse_httpStatus = Lens.lens (\PutChannelPolicyResponse' {httpStatus} -> httpStatus) (\s@PutChannelPolicyResponse' {} a -> s {httpStatus = a} :: PutChannelPolicyResponse)

instance Prelude.NFData PutChannelPolicyResponse where
  rnf PutChannelPolicyResponse' {..} =
    Prelude.rnf httpStatus
