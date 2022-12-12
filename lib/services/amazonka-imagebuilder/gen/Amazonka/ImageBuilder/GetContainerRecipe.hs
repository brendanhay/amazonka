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
-- Module      : Amazonka.ImageBuilder.GetContainerRecipe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a container recipe.
module Amazonka.ImageBuilder.GetContainerRecipe
  ( -- * Creating a Request
    GetContainerRecipe (..),
    newGetContainerRecipe,

    -- * Request Lenses
    getContainerRecipe_containerRecipeArn,

    -- * Destructuring the Response
    GetContainerRecipeResponse (..),
    newGetContainerRecipeResponse,

    -- * Response Lenses
    getContainerRecipeResponse_containerRecipe,
    getContainerRecipeResponse_requestId,
    getContainerRecipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerRecipe' smart constructor.
data GetContainerRecipe = GetContainerRecipe'
  { -- | The Amazon Resource Name (ARN) of the container recipe to retrieve.
    containerRecipeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerRecipeArn', 'getContainerRecipe_containerRecipeArn' - The Amazon Resource Name (ARN) of the container recipe to retrieve.
newGetContainerRecipe ::
  -- | 'containerRecipeArn'
  Prelude.Text ->
  GetContainerRecipe
newGetContainerRecipe pContainerRecipeArn_ =
  GetContainerRecipe'
    { containerRecipeArn =
        pContainerRecipeArn_
    }

-- | The Amazon Resource Name (ARN) of the container recipe to retrieve.
getContainerRecipe_containerRecipeArn :: Lens.Lens' GetContainerRecipe Prelude.Text
getContainerRecipe_containerRecipeArn = Lens.lens (\GetContainerRecipe' {containerRecipeArn} -> containerRecipeArn) (\s@GetContainerRecipe' {} a -> s {containerRecipeArn = a} :: GetContainerRecipe)

instance Core.AWSRequest GetContainerRecipe where
  type
    AWSResponse GetContainerRecipe =
      GetContainerRecipeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerRecipeResponse'
            Prelude.<$> (x Data..?> "containerRecipe")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerRecipe where
  hashWithSalt _salt GetContainerRecipe' {..} =
    _salt `Prelude.hashWithSalt` containerRecipeArn

instance Prelude.NFData GetContainerRecipe where
  rnf GetContainerRecipe' {..} =
    Prelude.rnf containerRecipeArn

instance Data.ToHeaders GetContainerRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetContainerRecipe where
  toPath = Prelude.const "/GetContainerRecipe"

instance Data.ToQuery GetContainerRecipe where
  toQuery GetContainerRecipe' {..} =
    Prelude.mconcat
      ["containerRecipeArn" Data.=: containerRecipeArn]

-- | /See:/ 'newGetContainerRecipeResponse' smart constructor.
data GetContainerRecipeResponse = GetContainerRecipeResponse'
  { -- | The container recipe object that is returned.
    containerRecipe :: Prelude.Maybe ContainerRecipe,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerRecipe', 'getContainerRecipeResponse_containerRecipe' - The container recipe object that is returned.
--
-- 'requestId', 'getContainerRecipeResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'getContainerRecipeResponse_httpStatus' - The response's http status code.
newGetContainerRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContainerRecipeResponse
newGetContainerRecipeResponse pHttpStatus_ =
  GetContainerRecipeResponse'
    { containerRecipe =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container recipe object that is returned.
getContainerRecipeResponse_containerRecipe :: Lens.Lens' GetContainerRecipeResponse (Prelude.Maybe ContainerRecipe)
getContainerRecipeResponse_containerRecipe = Lens.lens (\GetContainerRecipeResponse' {containerRecipe} -> containerRecipe) (\s@GetContainerRecipeResponse' {} a -> s {containerRecipe = a} :: GetContainerRecipeResponse)

-- | The request ID that uniquely identifies this request.
getContainerRecipeResponse_requestId :: Lens.Lens' GetContainerRecipeResponse (Prelude.Maybe Prelude.Text)
getContainerRecipeResponse_requestId = Lens.lens (\GetContainerRecipeResponse' {requestId} -> requestId) (\s@GetContainerRecipeResponse' {} a -> s {requestId = a} :: GetContainerRecipeResponse)

-- | The response's http status code.
getContainerRecipeResponse_httpStatus :: Lens.Lens' GetContainerRecipeResponse Prelude.Int
getContainerRecipeResponse_httpStatus = Lens.lens (\GetContainerRecipeResponse' {httpStatus} -> httpStatus) (\s@GetContainerRecipeResponse' {} a -> s {httpStatus = a} :: GetContainerRecipeResponse)

instance Prelude.NFData GetContainerRecipeResponse where
  rnf GetContainerRecipeResponse' {..} =
    Prelude.rnf containerRecipe
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
