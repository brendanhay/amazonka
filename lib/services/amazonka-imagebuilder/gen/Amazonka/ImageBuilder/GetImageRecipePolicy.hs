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
-- Module      : Amazonka.ImageBuilder.GetImageRecipePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an image recipe policy.
module Amazonka.ImageBuilder.GetImageRecipePolicy
  ( -- * Creating a Request
    GetImageRecipePolicy (..),
    newGetImageRecipePolicy,

    -- * Request Lenses
    getImageRecipePolicy_imageRecipeArn,

    -- * Destructuring the Response
    GetImageRecipePolicyResponse (..),
    newGetImageRecipePolicyResponse,

    -- * Response Lenses
    getImageRecipePolicyResponse_policy,
    getImageRecipePolicyResponse_requestId,
    getImageRecipePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImageRecipePolicy' smart constructor.
data GetImageRecipePolicy = GetImageRecipePolicy'
  { -- | The Amazon Resource Name (ARN) of the image recipe whose policy you want
    -- to retrieve.
    imageRecipeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImageRecipePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageRecipeArn', 'getImageRecipePolicy_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe whose policy you want
-- to retrieve.
newGetImageRecipePolicy ::
  -- | 'imageRecipeArn'
  Prelude.Text ->
  GetImageRecipePolicy
newGetImageRecipePolicy pImageRecipeArn_ =
  GetImageRecipePolicy'
    { imageRecipeArn =
        pImageRecipeArn_
    }

-- | The Amazon Resource Name (ARN) of the image recipe whose policy you want
-- to retrieve.
getImageRecipePolicy_imageRecipeArn :: Lens.Lens' GetImageRecipePolicy Prelude.Text
getImageRecipePolicy_imageRecipeArn = Lens.lens (\GetImageRecipePolicy' {imageRecipeArn} -> imageRecipeArn) (\s@GetImageRecipePolicy' {} a -> s {imageRecipeArn = a} :: GetImageRecipePolicy)

instance Core.AWSRequest GetImageRecipePolicy where
  type
    AWSResponse GetImageRecipePolicy =
      GetImageRecipePolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImageRecipePolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImageRecipePolicy where
  hashWithSalt _salt GetImageRecipePolicy' {..} =
    _salt `Prelude.hashWithSalt` imageRecipeArn

instance Prelude.NFData GetImageRecipePolicy where
  rnf GetImageRecipePolicy' {..} =
    Prelude.rnf imageRecipeArn

instance Data.ToHeaders GetImageRecipePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetImageRecipePolicy where
  toPath = Prelude.const "/GetImageRecipePolicy"

instance Data.ToQuery GetImageRecipePolicy where
  toQuery GetImageRecipePolicy' {..} =
    Prelude.mconcat
      ["imageRecipeArn" Data.=: imageRecipeArn]

-- | /See:/ 'newGetImageRecipePolicyResponse' smart constructor.
data GetImageRecipePolicyResponse = GetImageRecipePolicyResponse'
  { -- | The image recipe policy object.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImageRecipePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getImageRecipePolicyResponse_policy' - The image recipe policy object.
--
-- 'requestId', 'getImageRecipePolicyResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'getImageRecipePolicyResponse_httpStatus' - The response's http status code.
newGetImageRecipePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImageRecipePolicyResponse
newGetImageRecipePolicyResponse pHttpStatus_ =
  GetImageRecipePolicyResponse'
    { policy =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image recipe policy object.
getImageRecipePolicyResponse_policy :: Lens.Lens' GetImageRecipePolicyResponse (Prelude.Maybe Prelude.Text)
getImageRecipePolicyResponse_policy = Lens.lens (\GetImageRecipePolicyResponse' {policy} -> policy) (\s@GetImageRecipePolicyResponse' {} a -> s {policy = a} :: GetImageRecipePolicyResponse)

-- | The request ID that uniquely identifies this request.
getImageRecipePolicyResponse_requestId :: Lens.Lens' GetImageRecipePolicyResponse (Prelude.Maybe Prelude.Text)
getImageRecipePolicyResponse_requestId = Lens.lens (\GetImageRecipePolicyResponse' {requestId} -> requestId) (\s@GetImageRecipePolicyResponse' {} a -> s {requestId = a} :: GetImageRecipePolicyResponse)

-- | The response's http status code.
getImageRecipePolicyResponse_httpStatus :: Lens.Lens' GetImageRecipePolicyResponse Prelude.Int
getImageRecipePolicyResponse_httpStatus = Lens.lens (\GetImageRecipePolicyResponse' {httpStatus} -> httpStatus) (\s@GetImageRecipePolicyResponse' {} a -> s {httpStatus = a} :: GetImageRecipePolicyResponse)

instance Prelude.NFData GetImageRecipePolicyResponse where
  rnf GetImageRecipePolicyResponse' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf httpStatus
