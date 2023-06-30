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
-- Module      : Amazonka.ServiceCatalog.DisassociateTagOptionFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified TagOption from the specified resource.
module Amazonka.ServiceCatalog.DisassociateTagOptionFromResource
  ( -- * Creating a Request
    DisassociateTagOptionFromResource (..),
    newDisassociateTagOptionFromResource,

    -- * Request Lenses
    disassociateTagOptionFromResource_resourceId,
    disassociateTagOptionFromResource_tagOptionId,

    -- * Destructuring the Response
    DisassociateTagOptionFromResourceResponse (..),
    newDisassociateTagOptionFromResourceResponse,

    -- * Response Lenses
    disassociateTagOptionFromResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDisassociateTagOptionFromResource' smart constructor.
data DisassociateTagOptionFromResource = DisassociateTagOptionFromResource'
  { -- | The resource identifier.
    resourceId :: Prelude.Text,
    -- | The TagOption identifier.
    tagOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTagOptionFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'disassociateTagOptionFromResource_resourceId' - The resource identifier.
--
-- 'tagOptionId', 'disassociateTagOptionFromResource_tagOptionId' - The TagOption identifier.
newDisassociateTagOptionFromResource ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'tagOptionId'
  Prelude.Text ->
  DisassociateTagOptionFromResource
newDisassociateTagOptionFromResource
  pResourceId_
  pTagOptionId_ =
    DisassociateTagOptionFromResource'
      { resourceId =
          pResourceId_,
        tagOptionId = pTagOptionId_
      }

-- | The resource identifier.
disassociateTagOptionFromResource_resourceId :: Lens.Lens' DisassociateTagOptionFromResource Prelude.Text
disassociateTagOptionFromResource_resourceId = Lens.lens (\DisassociateTagOptionFromResource' {resourceId} -> resourceId) (\s@DisassociateTagOptionFromResource' {} a -> s {resourceId = a} :: DisassociateTagOptionFromResource)

-- | The TagOption identifier.
disassociateTagOptionFromResource_tagOptionId :: Lens.Lens' DisassociateTagOptionFromResource Prelude.Text
disassociateTagOptionFromResource_tagOptionId = Lens.lens (\DisassociateTagOptionFromResource' {tagOptionId} -> tagOptionId) (\s@DisassociateTagOptionFromResource' {} a -> s {tagOptionId = a} :: DisassociateTagOptionFromResource)

instance
  Core.AWSRequest
    DisassociateTagOptionFromResource
  where
  type
    AWSResponse DisassociateTagOptionFromResource =
      DisassociateTagOptionFromResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateTagOptionFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateTagOptionFromResource
  where
  hashWithSalt
    _salt
    DisassociateTagOptionFromResource' {..} =
      _salt
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` tagOptionId

instance
  Prelude.NFData
    DisassociateTagOptionFromResource
  where
  rnf DisassociateTagOptionFromResource' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tagOptionId

instance
  Data.ToHeaders
    DisassociateTagOptionFromResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DisassociateTagOptionFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateTagOptionFromResource
  where
  toJSON DisassociateTagOptionFromResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("TagOptionId" Data..= tagOptionId)
          ]
      )

instance
  Data.ToPath
    DisassociateTagOptionFromResource
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateTagOptionFromResource
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateTagOptionFromResourceResponse' smart constructor.
data DisassociateTagOptionFromResourceResponse = DisassociateTagOptionFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTagOptionFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateTagOptionFromResourceResponse_httpStatus' - The response's http status code.
newDisassociateTagOptionFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTagOptionFromResourceResponse
newDisassociateTagOptionFromResourceResponse
  pHttpStatus_ =
    DisassociateTagOptionFromResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateTagOptionFromResourceResponse_httpStatus :: Lens.Lens' DisassociateTagOptionFromResourceResponse Prelude.Int
disassociateTagOptionFromResourceResponse_httpStatus = Lens.lens (\DisassociateTagOptionFromResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateTagOptionFromResourceResponse' {} a -> s {httpStatus = a} :: DisassociateTagOptionFromResourceResponse)

instance
  Prelude.NFData
    DisassociateTagOptionFromResourceResponse
  where
  rnf DisassociateTagOptionFromResourceResponse' {..} =
    Prelude.rnf httpStatus
