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
-- Module      : Network.AWS.Route53Domains.UpdateTagsForDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds or updates tags for a specified domain.
--
-- All tag operations are eventually consistent; subsequent operations
-- might not immediately represent all issued operations.
module Network.AWS.Route53Domains.UpdateTagsForDomain
  ( -- * Creating a Request
    UpdateTagsForDomain (..),
    newUpdateTagsForDomain,

    -- * Request Lenses
    updateTagsForDomain_tagsToUpdate,
    updateTagsForDomain_domainName,

    -- * Destructuring the Response
    UpdateTagsForDomainResponse (..),
    newUpdateTagsForDomainResponse,

    -- * Response Lenses
    updateTagsForDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The UpdateTagsForDomainRequest includes the following elements.
--
-- /See:/ 'newUpdateTagsForDomain' smart constructor.
data UpdateTagsForDomain = UpdateTagsForDomain'
  { -- | A list of the tag keys and values that you want to add or update. If you
    -- specify a key that already exists, the corresponding value will be
    -- replaced.
    tagsToUpdate :: Core.Maybe [Tag],
    -- | The domain for which you want to add or update tags.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTagsForDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagsToUpdate', 'updateTagsForDomain_tagsToUpdate' - A list of the tag keys and values that you want to add or update. If you
-- specify a key that already exists, the corresponding value will be
-- replaced.
--
-- 'domainName', 'updateTagsForDomain_domainName' - The domain for which you want to add or update tags.
newUpdateTagsForDomain ::
  -- | 'domainName'
  Core.Text ->
  UpdateTagsForDomain
newUpdateTagsForDomain pDomainName_ =
  UpdateTagsForDomain'
    { tagsToUpdate = Core.Nothing,
      domainName = pDomainName_
    }

-- | A list of the tag keys and values that you want to add or update. If you
-- specify a key that already exists, the corresponding value will be
-- replaced.
updateTagsForDomain_tagsToUpdate :: Lens.Lens' UpdateTagsForDomain (Core.Maybe [Tag])
updateTagsForDomain_tagsToUpdate = Lens.lens (\UpdateTagsForDomain' {tagsToUpdate} -> tagsToUpdate) (\s@UpdateTagsForDomain' {} a -> s {tagsToUpdate = a} :: UpdateTagsForDomain) Core.. Lens.mapping Lens._Coerce

-- | The domain for which you want to add or update tags.
updateTagsForDomain_domainName :: Lens.Lens' UpdateTagsForDomain Core.Text
updateTagsForDomain_domainName = Lens.lens (\UpdateTagsForDomain' {domainName} -> domainName) (\s@UpdateTagsForDomain' {} a -> s {domainName = a} :: UpdateTagsForDomain)

instance Core.AWSRequest UpdateTagsForDomain where
  type
    AWSResponse UpdateTagsForDomain =
      UpdateTagsForDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTagsForDomainResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTagsForDomain

instance Core.NFData UpdateTagsForDomain

instance Core.ToHeaders UpdateTagsForDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.UpdateTagsForDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTagsForDomain where
  toJSON UpdateTagsForDomain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagsToUpdate" Core..=) Core.<$> tagsToUpdate,
            Core.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath UpdateTagsForDomain where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTagsForDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTagsForDomainResponse' smart constructor.
data UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTagsForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTagsForDomainResponse_httpStatus' - The response's http status code.
newUpdateTagsForDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTagsForDomainResponse
newUpdateTagsForDomainResponse pHttpStatus_ =
  UpdateTagsForDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTagsForDomainResponse_httpStatus :: Lens.Lens' UpdateTagsForDomainResponse Core.Int
updateTagsForDomainResponse_httpStatus = Lens.lens (\UpdateTagsForDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateTagsForDomainResponse' {} a -> s {httpStatus = a} :: UpdateTagsForDomainResponse)

instance Core.NFData UpdateTagsForDomainResponse
