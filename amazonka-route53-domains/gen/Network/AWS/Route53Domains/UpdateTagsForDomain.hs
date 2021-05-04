{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    tagsToUpdate :: Prelude.Maybe [Tag],
    -- | The domain for which you want to add or update tags.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
updateTagsForDomain_tagsToUpdate = Lens.lens (\UpdateTagsForDomain' {tagsToUpdate} -> tagsToUpdate) (\s@UpdateTagsForDomain' {} a -> s {tagsToUpdate = a} :: UpdateTagsForDomain) Prelude.. Lens.mapping Prelude._Coerce

-- | The domain for which you want to add or update tags.
updateTagsForDomain_domainName :: Lens.Lens' UpdateTagsForDomain Prelude.Text
updateTagsForDomain_domainName = Lens.lens (\UpdateTagsForDomain' {domainName} -> domainName) (\s@UpdateTagsForDomain' {} a -> s {domainName = a} :: UpdateTagsForDomain)

instance Prelude.AWSRequest UpdateTagsForDomain where
  type
    Rs UpdateTagsForDomain =
      UpdateTagsForDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTagsForDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTagsForDomain

instance Prelude.NFData UpdateTagsForDomain

instance Prelude.ToHeaders UpdateTagsForDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.UpdateTagsForDomain" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTagsForDomain where
  toJSON UpdateTagsForDomain' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TagsToUpdate" Prelude..=)
              Prelude.<$> tagsToUpdate,
            Prelude.Just ("DomainName" Prelude..= domainName)
          ]
      )

instance Prelude.ToPath UpdateTagsForDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTagsForDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTagsForDomainResponse' smart constructor.
data UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateTagsForDomainResponse
