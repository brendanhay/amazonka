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
-- Module      : Amazonka.MediaTailor.GetChannelPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the channel\'s IAM policy. IAM policies are used to control
-- access to your channel.
module Amazonka.MediaTailor.GetChannelPolicy
  ( -- * Creating a Request
    GetChannelPolicy (..),
    newGetChannelPolicy,

    -- * Request Lenses
    getChannelPolicy_channelName,

    -- * Destructuring the Response
    GetChannelPolicyResponse (..),
    newGetChannelPolicyResponse,

    -- * Response Lenses
    getChannelPolicyResponse_policy,
    getChannelPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelPolicy' smart constructor.
data GetChannelPolicy = GetChannelPolicy'
  { -- | The name of the channel associated with this Channel Policy.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'getChannelPolicy_channelName' - The name of the channel associated with this Channel Policy.
newGetChannelPolicy ::
  -- | 'channelName'
  Prelude.Text ->
  GetChannelPolicy
newGetChannelPolicy pChannelName_ =
  GetChannelPolicy' {channelName = pChannelName_}

-- | The name of the channel associated with this Channel Policy.
getChannelPolicy_channelName :: Lens.Lens' GetChannelPolicy Prelude.Text
getChannelPolicy_channelName = Lens.lens (\GetChannelPolicy' {channelName} -> channelName) (\s@GetChannelPolicy' {} a -> s {channelName = a} :: GetChannelPolicy)

instance Core.AWSRequest GetChannelPolicy where
  type
    AWSResponse GetChannelPolicy =
      GetChannelPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelPolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannelPolicy where
  hashWithSalt _salt GetChannelPolicy' {..} =
    _salt `Prelude.hashWithSalt` channelName

instance Prelude.NFData GetChannelPolicy where
  rnf GetChannelPolicy' {..} = Prelude.rnf channelName

instance Core.ToHeaders GetChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetChannelPolicy where
  toPath GetChannelPolicy' {..} =
    Prelude.mconcat
      ["/channel/", Core.toBS channelName, "/policy"]

instance Core.ToQuery GetChannelPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelPolicyResponse' smart constructor.
data GetChannelPolicyResponse = GetChannelPolicyResponse'
  { -- | The IAM policy for the channel. IAM policies are used to control access
    -- to your channel.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getChannelPolicyResponse_policy' - The IAM policy for the channel. IAM policies are used to control access
-- to your channel.
--
-- 'httpStatus', 'getChannelPolicyResponse_httpStatus' - The response's http status code.
newGetChannelPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelPolicyResponse
newGetChannelPolicyResponse pHttpStatus_ =
  GetChannelPolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM policy for the channel. IAM policies are used to control access
-- to your channel.
getChannelPolicyResponse_policy :: Lens.Lens' GetChannelPolicyResponse (Prelude.Maybe Prelude.Text)
getChannelPolicyResponse_policy = Lens.lens (\GetChannelPolicyResponse' {policy} -> policy) (\s@GetChannelPolicyResponse' {} a -> s {policy = a} :: GetChannelPolicyResponse)

-- | The response's http status code.
getChannelPolicyResponse_httpStatus :: Lens.Lens' GetChannelPolicyResponse Prelude.Int
getChannelPolicyResponse_httpStatus = Lens.lens (\GetChannelPolicyResponse' {httpStatus} -> httpStatus) (\s@GetChannelPolicyResponse' {} a -> s {httpStatus = a} :: GetChannelPolicyResponse)

instance Prelude.NFData GetChannelPolicyResponse where
  rnf GetChannelPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
