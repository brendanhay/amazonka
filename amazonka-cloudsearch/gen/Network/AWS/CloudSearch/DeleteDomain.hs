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
-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain
-- has been deleted, it cannot be recovered. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_domainName,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,

    -- * Response Lenses
    deleteDomainResponse_domainStatus,
    deleteDomainResponse_httpStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DeleteDomain@ operation. Specifies
-- the name of the domain you want to delete.
--
-- /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The name of the domain you want to permanently delete.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomain_domainName' - The name of the domain you want to permanently delete.
newDeleteDomain ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteDomain
newDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | The name of the domain you want to permanently delete.
deleteDomain_domainName :: Lens.Lens' DeleteDomain Prelude.Text
deleteDomain_domainName = Lens.lens (\DeleteDomain' {domainName} -> domainName) (\s@DeleteDomain' {} a -> s {domainName = a} :: DeleteDomain)

instance Prelude.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteDomainResult"
      ( \s h x ->
          DeleteDomainResponse'
            Prelude.<$> (x Prelude..@? "DomainStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDomain

instance Prelude.NFData DeleteDomain

instance Prelude.ToHeaders DeleteDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDomain where
  toQuery DeleteDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteDomain" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName
      ]

-- | The result of a @DeleteDomain@ request. Contains the status of a newly
-- deleted domain, or no status if the domain has already been completely
-- deleted.
--
-- /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { domainStatus :: Prelude.Maybe DomainStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'deleteDomainResponse_domainStatus' - Undocumented member.
--
-- 'httpStatus', 'deleteDomainResponse_httpStatus' - The response's http status code.
newDeleteDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDomainResponse
newDeleteDomainResponse pHttpStatus_ =
  DeleteDomainResponse'
    { domainStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDomainResponse_domainStatus :: Lens.Lens' DeleteDomainResponse (Prelude.Maybe DomainStatus)
deleteDomainResponse_domainStatus = Lens.lens (\DeleteDomainResponse' {domainStatus} -> domainStatus) (\s@DeleteDomainResponse' {} a -> s {domainStatus = a} :: DeleteDomainResponse)

-- | The response's http status code.
deleteDomainResponse_httpStatus :: Lens.Lens' DeleteDomainResponse Prelude.Int
deleteDomainResponse_httpStatus = Lens.lens (\DeleteDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainResponse' {} a -> s {httpStatus = a} :: DeleteDomainResponse)

instance Prelude.NFData DeleteDomainResponse
