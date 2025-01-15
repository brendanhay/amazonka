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
-- Module      : Amazonka.Route53Domains.DeleteTagsForDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations
-- might not immediately represent all issued operations.
module Amazonka.Route53Domains.DeleteTagsForDomain
  ( -- * Creating a Request
    DeleteTagsForDomain (..),
    newDeleteTagsForDomain,

    -- * Request Lenses
    deleteTagsForDomain_domainName,
    deleteTagsForDomain_tagsToDelete,

    -- * Destructuring the Response
    DeleteTagsForDomainResponse (..),
    newDeleteTagsForDomainResponse,

    -- * Response Lenses
    deleteTagsForDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The DeleteTagsForDomainRequest includes the following elements.
--
-- /See:/ 'newDeleteTagsForDomain' smart constructor.
data DeleteTagsForDomain = DeleteTagsForDomain'
  { -- | The domain for which you want to delete one or more tags.
    domainName :: Prelude.Text,
    -- | A list of tag keys to delete.
    tagsToDelete :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTagsForDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteTagsForDomain_domainName' - The domain for which you want to delete one or more tags.
--
-- 'tagsToDelete', 'deleteTagsForDomain_tagsToDelete' - A list of tag keys to delete.
newDeleteTagsForDomain ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteTagsForDomain
newDeleteTagsForDomain pDomainName_ =
  DeleteTagsForDomain'
    { domainName = pDomainName_,
      tagsToDelete = Prelude.mempty
    }

-- | The domain for which you want to delete one or more tags.
deleteTagsForDomain_domainName :: Lens.Lens' DeleteTagsForDomain Prelude.Text
deleteTagsForDomain_domainName = Lens.lens (\DeleteTagsForDomain' {domainName} -> domainName) (\s@DeleteTagsForDomain' {} a -> s {domainName = a} :: DeleteTagsForDomain)

-- | A list of tag keys to delete.
deleteTagsForDomain_tagsToDelete :: Lens.Lens' DeleteTagsForDomain [Prelude.Text]
deleteTagsForDomain_tagsToDelete = Lens.lens (\DeleteTagsForDomain' {tagsToDelete} -> tagsToDelete) (\s@DeleteTagsForDomain' {} a -> s {tagsToDelete = a} :: DeleteTagsForDomain) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteTagsForDomain where
  type
    AWSResponse DeleteTagsForDomain =
      DeleteTagsForDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsForDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTagsForDomain where
  hashWithSalt _salt DeleteTagsForDomain' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` tagsToDelete

instance Prelude.NFData DeleteTagsForDomain where
  rnf DeleteTagsForDomain' {..} =
    Prelude.rnf domainName `Prelude.seq`
      Prelude.rnf tagsToDelete

instance Data.ToHeaders DeleteTagsForDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.DeleteTagsForDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTagsForDomain where
  toJSON DeleteTagsForDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just ("TagsToDelete" Data..= tagsToDelete)
          ]
      )

instance Data.ToPath DeleteTagsForDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTagsForDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTagsForDomainResponse' smart constructor.
data DeleteTagsForDomainResponse = DeleteTagsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTagsForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTagsForDomainResponse_httpStatus' - The response's http status code.
newDeleteTagsForDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTagsForDomainResponse
newDeleteTagsForDomainResponse pHttpStatus_ =
  DeleteTagsForDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTagsForDomainResponse_httpStatus :: Lens.Lens' DeleteTagsForDomainResponse Prelude.Int
deleteTagsForDomainResponse_httpStatus = Lens.lens (\DeleteTagsForDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteTagsForDomainResponse' {} a -> s {httpStatus = a} :: DeleteTagsForDomainResponse)

instance Prelude.NFData DeleteTagsForDomainResponse where
  rnf DeleteTagsForDomainResponse' {..} =
    Prelude.rnf httpStatus
