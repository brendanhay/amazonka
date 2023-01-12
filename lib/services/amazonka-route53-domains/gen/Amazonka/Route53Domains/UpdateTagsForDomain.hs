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
-- Module      : Amazonka.Route53Domains.UpdateTagsForDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds or updates tags for a specified domain.
--
-- All tag operations are eventually consistent; subsequent operations
-- might not immediately represent all issued operations.
module Amazonka.Route53Domains.UpdateTagsForDomain
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The UpdateTagsForDomainRequest includes the following elements.
--
-- /See:/ 'newUpdateTagsForDomain' smart constructor.
data UpdateTagsForDomain = UpdateTagsForDomain'
  { -- | A list of the tag keys and values that you want to add or update. If you
    -- specify a key that already exists, the corresponding value will be
    -- replaced.
    tagsToUpdate :: Prelude.Maybe [Tag],
    -- | The domain for which you want to add or update tags.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateTagsForDomain
newUpdateTagsForDomain pDomainName_ =
  UpdateTagsForDomain'
    { tagsToUpdate =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | A list of the tag keys and values that you want to add or update. If you
-- specify a key that already exists, the corresponding value will be
-- replaced.
updateTagsForDomain_tagsToUpdate :: Lens.Lens' UpdateTagsForDomain (Prelude.Maybe [Tag])
updateTagsForDomain_tagsToUpdate = Lens.lens (\UpdateTagsForDomain' {tagsToUpdate} -> tagsToUpdate) (\s@UpdateTagsForDomain' {} a -> s {tagsToUpdate = a} :: UpdateTagsForDomain) Prelude.. Lens.mapping Lens.coerced

-- | The domain for which you want to add or update tags.
updateTagsForDomain_domainName :: Lens.Lens' UpdateTagsForDomain Prelude.Text
updateTagsForDomain_domainName = Lens.lens (\UpdateTagsForDomain' {domainName} -> domainName) (\s@UpdateTagsForDomain' {} a -> s {domainName = a} :: UpdateTagsForDomain)

instance Core.AWSRequest UpdateTagsForDomain where
  type
    AWSResponse UpdateTagsForDomain =
      UpdateTagsForDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTagsForDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTagsForDomain where
  hashWithSalt _salt UpdateTagsForDomain' {..} =
    _salt `Prelude.hashWithSalt` tagsToUpdate
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateTagsForDomain where
  rnf UpdateTagsForDomain' {..} =
    Prelude.rnf tagsToUpdate
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateTagsForDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.UpdateTagsForDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTagsForDomain where
  toJSON UpdateTagsForDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TagsToUpdate" Data..=) Prelude.<$> tagsToUpdate,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath UpdateTagsForDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTagsForDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTagsForDomainResponse' smart constructor.
data UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTagsForDomainResponse
newUpdateTagsForDomainResponse pHttpStatus_ =
  UpdateTagsForDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTagsForDomainResponse_httpStatus :: Lens.Lens' UpdateTagsForDomainResponse Prelude.Int
updateTagsForDomainResponse_httpStatus = Lens.lens (\UpdateTagsForDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateTagsForDomainResponse' {} a -> s {httpStatus = a} :: UpdateTagsForDomainResponse)

instance Prelude.NFData UpdateTagsForDomainResponse where
  rnf UpdateTagsForDomainResponse' {..} =
    Prelude.rnf httpStatus
