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
-- Module      : Amazonka.ImageBuilder.GetImagePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an image policy.
module Amazonka.ImageBuilder.GetImagePolicy
  ( -- * Creating a Request
    GetImagePolicy (..),
    newGetImagePolicy,

    -- * Request Lenses
    getImagePolicy_imageArn,

    -- * Destructuring the Response
    GetImagePolicyResponse (..),
    newGetImagePolicyResponse,

    -- * Response Lenses
    getImagePolicyResponse_policy,
    getImagePolicyResponse_requestId,
    getImagePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImagePolicy' smart constructor.
data GetImagePolicy = GetImagePolicy'
  { -- | The Amazon Resource Name (ARN) of the image whose policy you want to
    -- retrieve.
    imageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImagePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageArn', 'getImagePolicy_imageArn' - The Amazon Resource Name (ARN) of the image whose policy you want to
-- retrieve.
newGetImagePolicy ::
  -- | 'imageArn'
  Prelude.Text ->
  GetImagePolicy
newGetImagePolicy pImageArn_ =
  GetImagePolicy' {imageArn = pImageArn_}

-- | The Amazon Resource Name (ARN) of the image whose policy you want to
-- retrieve.
getImagePolicy_imageArn :: Lens.Lens' GetImagePolicy Prelude.Text
getImagePolicy_imageArn = Lens.lens (\GetImagePolicy' {imageArn} -> imageArn) (\s@GetImagePolicy' {} a -> s {imageArn = a} :: GetImagePolicy)

instance Core.AWSRequest GetImagePolicy where
  type
    AWSResponse GetImagePolicy =
      GetImagePolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImagePolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImagePolicy where
  hashWithSalt _salt GetImagePolicy' {..} =
    _salt `Prelude.hashWithSalt` imageArn

instance Prelude.NFData GetImagePolicy where
  rnf GetImagePolicy' {..} = Prelude.rnf imageArn

instance Data.ToHeaders GetImagePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetImagePolicy where
  toPath = Prelude.const "/GetImagePolicy"

instance Data.ToQuery GetImagePolicy where
  toQuery GetImagePolicy' {..} =
    Prelude.mconcat ["imageArn" Data.=: imageArn]

-- | /See:/ 'newGetImagePolicyResponse' smart constructor.
data GetImagePolicyResponse = GetImagePolicyResponse'
  { -- | The image policy object.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImagePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getImagePolicyResponse_policy' - The image policy object.
--
-- 'requestId', 'getImagePolicyResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'getImagePolicyResponse_httpStatus' - The response's http status code.
newGetImagePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImagePolicyResponse
newGetImagePolicyResponse pHttpStatus_ =
  GetImagePolicyResponse'
    { policy = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image policy object.
getImagePolicyResponse_policy :: Lens.Lens' GetImagePolicyResponse (Prelude.Maybe Prelude.Text)
getImagePolicyResponse_policy = Lens.lens (\GetImagePolicyResponse' {policy} -> policy) (\s@GetImagePolicyResponse' {} a -> s {policy = a} :: GetImagePolicyResponse)

-- | The request ID that uniquely identifies this request.
getImagePolicyResponse_requestId :: Lens.Lens' GetImagePolicyResponse (Prelude.Maybe Prelude.Text)
getImagePolicyResponse_requestId = Lens.lens (\GetImagePolicyResponse' {requestId} -> requestId) (\s@GetImagePolicyResponse' {} a -> s {requestId = a} :: GetImagePolicyResponse)

-- | The response's http status code.
getImagePolicyResponse_httpStatus :: Lens.Lens' GetImagePolicyResponse Prelude.Int
getImagePolicyResponse_httpStatus = Lens.lens (\GetImagePolicyResponse' {httpStatus} -> httpStatus) (\s@GetImagePolicyResponse' {} a -> s {httpStatus = a} :: GetImagePolicyResponse)

instance Prelude.NFData GetImagePolicyResponse where
  rnf GetImagePolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
