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
-- Module      : Amazonka.ServiceCatalog.AssociateTagOptionWithResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate the specified TagOption with the specified portfolio or
-- product.
module Amazonka.ServiceCatalog.AssociateTagOptionWithResource
  ( -- * Creating a Request
    AssociateTagOptionWithResource (..),
    newAssociateTagOptionWithResource,

    -- * Request Lenses
    associateTagOptionWithResource_resourceId,
    associateTagOptionWithResource_tagOptionId,

    -- * Destructuring the Response
    AssociateTagOptionWithResourceResponse (..),
    newAssociateTagOptionWithResourceResponse,

    -- * Response Lenses
    associateTagOptionWithResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newAssociateTagOptionWithResource' smart constructor.
data AssociateTagOptionWithResource = AssociateTagOptionWithResource'
  { -- | The resource identifier.
    resourceId :: Prelude.Text,
    -- | The TagOption identifier.
    tagOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTagOptionWithResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'associateTagOptionWithResource_resourceId' - The resource identifier.
--
-- 'tagOptionId', 'associateTagOptionWithResource_tagOptionId' - The TagOption identifier.
newAssociateTagOptionWithResource ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'tagOptionId'
  Prelude.Text ->
  AssociateTagOptionWithResource
newAssociateTagOptionWithResource
  pResourceId_
  pTagOptionId_ =
    AssociateTagOptionWithResource'
      { resourceId =
          pResourceId_,
        tagOptionId = pTagOptionId_
      }

-- | The resource identifier.
associateTagOptionWithResource_resourceId :: Lens.Lens' AssociateTagOptionWithResource Prelude.Text
associateTagOptionWithResource_resourceId = Lens.lens (\AssociateTagOptionWithResource' {resourceId} -> resourceId) (\s@AssociateTagOptionWithResource' {} a -> s {resourceId = a} :: AssociateTagOptionWithResource)

-- | The TagOption identifier.
associateTagOptionWithResource_tagOptionId :: Lens.Lens' AssociateTagOptionWithResource Prelude.Text
associateTagOptionWithResource_tagOptionId = Lens.lens (\AssociateTagOptionWithResource' {tagOptionId} -> tagOptionId) (\s@AssociateTagOptionWithResource' {} a -> s {tagOptionId = a} :: AssociateTagOptionWithResource)

instance
  Core.AWSRequest
    AssociateTagOptionWithResource
  where
  type
    AWSResponse AssociateTagOptionWithResource =
      AssociateTagOptionWithResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateTagOptionWithResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateTagOptionWithResource
  where
  hashWithSalt
    _salt
    AssociateTagOptionWithResource' {..} =
      _salt
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` tagOptionId

instance
  Prelude.NFData
    AssociateTagOptionWithResource
  where
  rnf AssociateTagOptionWithResource' {..} =
    Prelude.rnf resourceId `Prelude.seq`
      Prelude.rnf tagOptionId

instance
  Data.ToHeaders
    AssociateTagOptionWithResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.AssociateTagOptionWithResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateTagOptionWithResource where
  toJSON AssociateTagOptionWithResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("TagOptionId" Data..= tagOptionId)
          ]
      )

instance Data.ToPath AssociateTagOptionWithResource where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateTagOptionWithResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateTagOptionWithResourceResponse' smart constructor.
data AssociateTagOptionWithResourceResponse = AssociateTagOptionWithResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTagOptionWithResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateTagOptionWithResourceResponse_httpStatus' - The response's http status code.
newAssociateTagOptionWithResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTagOptionWithResourceResponse
newAssociateTagOptionWithResourceResponse
  pHttpStatus_ =
    AssociateTagOptionWithResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateTagOptionWithResourceResponse_httpStatus :: Lens.Lens' AssociateTagOptionWithResourceResponse Prelude.Int
associateTagOptionWithResourceResponse_httpStatus = Lens.lens (\AssociateTagOptionWithResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateTagOptionWithResourceResponse' {} a -> s {httpStatus = a} :: AssociateTagOptionWithResourceResponse)

instance
  Prelude.NFData
    AssociateTagOptionWithResourceResponse
  where
  rnf AssociateTagOptionWithResourceResponse' {..} =
    Prelude.rnf httpStatus
