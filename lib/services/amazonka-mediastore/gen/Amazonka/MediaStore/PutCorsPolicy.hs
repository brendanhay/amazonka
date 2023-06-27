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
-- Module      : Amazonka.MediaStore.PutCorsPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the cross-origin resource sharing (CORS) configuration on a
-- container so that the container can service cross-origin requests. For
-- example, you might want to enable a request whose origin is
-- http:\/\/www.example.com to access your AWS Elemental MediaStore
-- container at my.example.container.com by using the browser\'s
-- XMLHttpRequest capability.
--
-- To enable CORS on a container, you attach a CORS policy to the
-- container. In the CORS policy, you configure rules that identify origins
-- and the HTTP methods that can be executed on your container. The policy
-- can contain up to 398,000 characters. You can add up to 100 rules to a
-- CORS policy. If more than one rule applies, the service uses the first
-- applicable rule listed.
--
-- To learn more about CORS, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/cors-policy.html Cross-Origin Resource Sharing (CORS) in AWS Elemental MediaStore>.
module Amazonka.MediaStore.PutCorsPolicy
  ( -- * Creating a Request
    PutCorsPolicy (..),
    newPutCorsPolicy,

    -- * Request Lenses
    putCorsPolicy_containerName,
    putCorsPolicy_corsPolicy,

    -- * Destructuring the Response
    PutCorsPolicyResponse (..),
    newPutCorsPolicyResponse,

    -- * Response Lenses
    putCorsPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutCorsPolicy' smart constructor.
data PutCorsPolicy = PutCorsPolicy'
  { -- | The name of the container that you want to assign the CORS policy to.
    containerName :: Prelude.Text,
    -- | The CORS policy to apply to the container.
    corsPolicy :: Prelude.NonEmpty CorsRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCorsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'putCorsPolicy_containerName' - The name of the container that you want to assign the CORS policy to.
--
-- 'corsPolicy', 'putCorsPolicy_corsPolicy' - The CORS policy to apply to the container.
newPutCorsPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  -- | 'corsPolicy'
  Prelude.NonEmpty CorsRule ->
  PutCorsPolicy
newPutCorsPolicy pContainerName_ pCorsPolicy_ =
  PutCorsPolicy'
    { containerName = pContainerName_,
      corsPolicy = Lens.coerced Lens.# pCorsPolicy_
    }

-- | The name of the container that you want to assign the CORS policy to.
putCorsPolicy_containerName :: Lens.Lens' PutCorsPolicy Prelude.Text
putCorsPolicy_containerName = Lens.lens (\PutCorsPolicy' {containerName} -> containerName) (\s@PutCorsPolicy' {} a -> s {containerName = a} :: PutCorsPolicy)

-- | The CORS policy to apply to the container.
putCorsPolicy_corsPolicy :: Lens.Lens' PutCorsPolicy (Prelude.NonEmpty CorsRule)
putCorsPolicy_corsPolicy = Lens.lens (\PutCorsPolicy' {corsPolicy} -> corsPolicy) (\s@PutCorsPolicy' {} a -> s {corsPolicy = a} :: PutCorsPolicy) Prelude.. Lens.coerced

instance Core.AWSRequest PutCorsPolicy where
  type
    AWSResponse PutCorsPolicy =
      PutCorsPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutCorsPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutCorsPolicy where
  hashWithSalt _salt PutCorsPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` corsPolicy

instance Prelude.NFData PutCorsPolicy where
  rnf PutCorsPolicy' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf corsPolicy

instance Data.ToHeaders PutCorsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.PutCorsPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutCorsPolicy where
  toJSON PutCorsPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName),
            Prelude.Just ("CorsPolicy" Data..= corsPolicy)
          ]
      )

instance Data.ToPath PutCorsPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutCorsPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutCorsPolicyResponse' smart constructor.
data PutCorsPolicyResponse = PutCorsPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCorsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putCorsPolicyResponse_httpStatus' - The response's http status code.
newPutCorsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutCorsPolicyResponse
newPutCorsPolicyResponse pHttpStatus_ =
  PutCorsPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putCorsPolicyResponse_httpStatus :: Lens.Lens' PutCorsPolicyResponse Prelude.Int
putCorsPolicyResponse_httpStatus = Lens.lens (\PutCorsPolicyResponse' {httpStatus} -> httpStatus) (\s@PutCorsPolicyResponse' {} a -> s {httpStatus = a} :: PutCorsPolicyResponse)

instance Prelude.NFData PutCorsPolicyResponse where
  rnf PutCorsPolicyResponse' {..} =
    Prelude.rnf httpStatus
