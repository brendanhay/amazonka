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
-- Module      : Network.AWS.MediaTailor.PutChannelPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM policy for the channel.
module Network.AWS.MediaTailor.PutChannelPolicy
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutChannelPolicy' smart constructor.
data PutChannelPolicy = PutChannelPolicy'
  { -- | The identifier for the channel you are working on.
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
-- 'channelName', 'putChannelPolicy_channelName' - The identifier for the channel you are working on.
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

-- | The identifier for the channel you are working on.
putChannelPolicy_channelName :: Lens.Lens' PutChannelPolicy Prelude.Text
putChannelPolicy_channelName = Lens.lens (\PutChannelPolicy' {channelName} -> channelName) (\s@PutChannelPolicy' {} a -> s {channelName = a} :: PutChannelPolicy)

-- | Adds an IAM role that determines the permissions of your channel.
putChannelPolicy_policy :: Lens.Lens' PutChannelPolicy Prelude.Text
putChannelPolicy_policy = Lens.lens (\PutChannelPolicy' {policy} -> policy) (\s@PutChannelPolicy' {} a -> s {policy = a} :: PutChannelPolicy)

instance Core.AWSRequest PutChannelPolicy where
  type
    AWSResponse PutChannelPolicy =
      PutChannelPolicyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutChannelPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutChannelPolicy

instance Prelude.NFData PutChannelPolicy

instance Core.ToHeaders PutChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutChannelPolicy where
  toJSON PutChannelPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Policy" Core..= policy)]
      )

instance Core.ToPath PutChannelPolicy where
  toPath PutChannelPolicy' {..} =
    Prelude.mconcat
      ["/channel/", Core.toBS channelName, "/policy"]

instance Core.ToQuery PutChannelPolicy where
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

instance Prelude.NFData PutChannelPolicyResponse
