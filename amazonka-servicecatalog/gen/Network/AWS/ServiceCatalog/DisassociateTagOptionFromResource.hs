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
-- Module      : Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified TagOption from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDisassociateTagOptionFromResource' smart constructor.
data DisassociateTagOptionFromResource = DisassociateTagOptionFromResource'
  { -- | The resource identifier.
    resourceId :: Core.Text,
    -- | The TagOption identifier.
    tagOptionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'tagOptionId'
  Core.Text ->
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
disassociateTagOptionFromResource_resourceId :: Lens.Lens' DisassociateTagOptionFromResource Core.Text
disassociateTagOptionFromResource_resourceId = Lens.lens (\DisassociateTagOptionFromResource' {resourceId} -> resourceId) (\s@DisassociateTagOptionFromResource' {} a -> s {resourceId = a} :: DisassociateTagOptionFromResource)

-- | The TagOption identifier.
disassociateTagOptionFromResource_tagOptionId :: Lens.Lens' DisassociateTagOptionFromResource Core.Text
disassociateTagOptionFromResource_tagOptionId = Lens.lens (\DisassociateTagOptionFromResource' {tagOptionId} -> tagOptionId) (\s@DisassociateTagOptionFromResource' {} a -> s {tagOptionId = a} :: DisassociateTagOptionFromResource)

instance
  Core.AWSRequest
    DisassociateTagOptionFromResource
  where
  type
    AWSResponse DisassociateTagOptionFromResource =
      DisassociateTagOptionFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateTagOptionFromResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateTagOptionFromResource

instance
  Core.NFData
    DisassociateTagOptionFromResource

instance
  Core.ToHeaders
    DisassociateTagOptionFromResource
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DisassociateTagOptionFromResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DisassociateTagOptionFromResource
  where
  toJSON DisassociateTagOptionFromResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("TagOptionId" Core..= tagOptionId)
          ]
      )

instance
  Core.ToPath
    DisassociateTagOptionFromResource
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateTagOptionFromResource
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateTagOptionFromResourceResponse' smart constructor.
data DisassociateTagOptionFromResourceResponse = DisassociateTagOptionFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateTagOptionFromResourceResponse
newDisassociateTagOptionFromResourceResponse
  pHttpStatus_ =
    DisassociateTagOptionFromResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateTagOptionFromResourceResponse_httpStatus :: Lens.Lens' DisassociateTagOptionFromResourceResponse Core.Int
disassociateTagOptionFromResourceResponse_httpStatus = Lens.lens (\DisassociateTagOptionFromResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateTagOptionFromResourceResponse' {} a -> s {httpStatus = a} :: DisassociateTagOptionFromResourceResponse)

instance
  Core.NFData
    DisassociateTagOptionFromResourceResponse
