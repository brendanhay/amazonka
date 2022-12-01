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
-- Module      : Amazonka.ImageBuilder.PutImageRecipePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a policy to an image recipe. We recommend that you call the RAM
-- API
-- <https://docs.aws.amazon.com/ram/latest/APIReference/API_CreateResourceShare.html CreateResourceShare>
-- to share resources. If you call the Image Builder API
-- @PutImageRecipePolicy@, you must also call the RAM API
-- <https://docs.aws.amazon.com/ram/latest/APIReference/API_PromoteResourceShareCreatedFromPolicy.html PromoteResourceShareCreatedFromPolicy>
-- in order for the resource to be visible to all principals with whom the
-- resource is shared.
module Amazonka.ImageBuilder.PutImageRecipePolicy
  ( -- * Creating a Request
    PutImageRecipePolicy (..),
    newPutImageRecipePolicy,

    -- * Request Lenses
    putImageRecipePolicy_imageRecipeArn,
    putImageRecipePolicy_policy,

    -- * Destructuring the Response
    PutImageRecipePolicyResponse (..),
    newPutImageRecipePolicyResponse,

    -- * Response Lenses
    putImageRecipePolicyResponse_requestId,
    putImageRecipePolicyResponse_imageRecipeArn,
    putImageRecipePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutImageRecipePolicy' smart constructor.
data PutImageRecipePolicy = PutImageRecipePolicy'
  { -- | The Amazon Resource Name (ARN) of the image recipe that this policy
    -- should be applied to.
    imageRecipeArn :: Prelude.Text,
    -- | The policy to apply.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutImageRecipePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageRecipeArn', 'putImageRecipePolicy_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that this policy
-- should be applied to.
--
-- 'policy', 'putImageRecipePolicy_policy' - The policy to apply.
newPutImageRecipePolicy ::
  -- | 'imageRecipeArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutImageRecipePolicy
newPutImageRecipePolicy pImageRecipeArn_ pPolicy_ =
  PutImageRecipePolicy'
    { imageRecipeArn =
        pImageRecipeArn_,
      policy = pPolicy_
    }

-- | The Amazon Resource Name (ARN) of the image recipe that this policy
-- should be applied to.
putImageRecipePolicy_imageRecipeArn :: Lens.Lens' PutImageRecipePolicy Prelude.Text
putImageRecipePolicy_imageRecipeArn = Lens.lens (\PutImageRecipePolicy' {imageRecipeArn} -> imageRecipeArn) (\s@PutImageRecipePolicy' {} a -> s {imageRecipeArn = a} :: PutImageRecipePolicy)

-- | The policy to apply.
putImageRecipePolicy_policy :: Lens.Lens' PutImageRecipePolicy Prelude.Text
putImageRecipePolicy_policy = Lens.lens (\PutImageRecipePolicy' {policy} -> policy) (\s@PutImageRecipePolicy' {} a -> s {policy = a} :: PutImageRecipePolicy)

instance Core.AWSRequest PutImageRecipePolicy where
  type
    AWSResponse PutImageRecipePolicy =
      PutImageRecipePolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutImageRecipePolicyResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "imageRecipeArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutImageRecipePolicy where
  hashWithSalt _salt PutImageRecipePolicy' {..} =
    _salt `Prelude.hashWithSalt` imageRecipeArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutImageRecipePolicy where
  rnf PutImageRecipePolicy' {..} =
    Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf policy

instance Core.ToHeaders PutImageRecipePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutImageRecipePolicy where
  toJSON PutImageRecipePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("imageRecipeArn" Core..= imageRecipeArn),
            Prelude.Just ("policy" Core..= policy)
          ]
      )

instance Core.ToPath PutImageRecipePolicy where
  toPath = Prelude.const "/PutImageRecipePolicy"

instance Core.ToQuery PutImageRecipePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutImageRecipePolicyResponse' smart constructor.
data PutImageRecipePolicyResponse = PutImageRecipePolicyResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image recipe that this policy was
    -- applied to.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutImageRecipePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'putImageRecipePolicyResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imageRecipeArn', 'putImageRecipePolicyResponse_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that this policy was
-- applied to.
--
-- 'httpStatus', 'putImageRecipePolicyResponse_httpStatus' - The response's http status code.
newPutImageRecipePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutImageRecipePolicyResponse
newPutImageRecipePolicyResponse pHttpStatus_ =
  PutImageRecipePolicyResponse'
    { requestId =
        Prelude.Nothing,
      imageRecipeArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
putImageRecipePolicyResponse_requestId :: Lens.Lens' PutImageRecipePolicyResponse (Prelude.Maybe Prelude.Text)
putImageRecipePolicyResponse_requestId = Lens.lens (\PutImageRecipePolicyResponse' {requestId} -> requestId) (\s@PutImageRecipePolicyResponse' {} a -> s {requestId = a} :: PutImageRecipePolicyResponse)

-- | The Amazon Resource Name (ARN) of the image recipe that this policy was
-- applied to.
putImageRecipePolicyResponse_imageRecipeArn :: Lens.Lens' PutImageRecipePolicyResponse (Prelude.Maybe Prelude.Text)
putImageRecipePolicyResponse_imageRecipeArn = Lens.lens (\PutImageRecipePolicyResponse' {imageRecipeArn} -> imageRecipeArn) (\s@PutImageRecipePolicyResponse' {} a -> s {imageRecipeArn = a} :: PutImageRecipePolicyResponse)

-- | The response's http status code.
putImageRecipePolicyResponse_httpStatus :: Lens.Lens' PutImageRecipePolicyResponse Prelude.Int
putImageRecipePolicyResponse_httpStatus = Lens.lens (\PutImageRecipePolicyResponse' {httpStatus} -> httpStatus) (\s@PutImageRecipePolicyResponse' {} a -> s {httpStatus = a} :: PutImageRecipePolicyResponse)

instance Prelude.NFData PutImageRecipePolicyResponse where
  rnf PutImageRecipePolicyResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf httpStatus
