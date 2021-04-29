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
-- Module      : Network.AWS.Route53Domains.DeleteTagsForDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations
-- might not immediately represent all issued operations.
module Network.AWS.Route53Domains.DeleteTagsForDomain
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The DeleteTagsForDomainRequest includes the following elements.
--
-- /See:/ 'newDeleteTagsForDomain' smart constructor.
data DeleteTagsForDomain = DeleteTagsForDomain'
  { -- | The domain for which you want to delete one or more tags.
    domainName :: Prelude.Text,
    -- | A list of tag keys to delete.
    tagsToDelete :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
deleteTagsForDomain_tagsToDelete = Lens.lens (\DeleteTagsForDomain' {tagsToDelete} -> tagsToDelete) (\s@DeleteTagsForDomain' {} a -> s {tagsToDelete = a} :: DeleteTagsForDomain) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DeleteTagsForDomain where
  type
    Rs DeleteTagsForDomain =
      DeleteTagsForDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsForDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTagsForDomain

instance Prelude.NFData DeleteTagsForDomain

instance Prelude.ToHeaders DeleteTagsForDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.DeleteTagsForDomain" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTagsForDomain where
  toJSON DeleteTagsForDomain' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Prelude..= domainName),
            Prelude.Just
              ("TagsToDelete" Prelude..= tagsToDelete)
          ]
      )

instance Prelude.ToPath DeleteTagsForDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTagsForDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTagsForDomainResponse' smart constructor.
data DeleteTagsForDomainResponse = DeleteTagsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteTagsForDomainResponse
