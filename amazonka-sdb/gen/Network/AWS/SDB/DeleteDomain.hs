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
-- Module      : Network.AWS.SDB.DeleteDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteDomain@ operation deletes a domain. Any items (and their
-- attributes) in the domain are deleted as well. The @DeleteDomain@
-- operation might take 10 or more seconds to complete.
module Network.AWS.SDB.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_domainName,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The name of the domain to delete.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomain_domainName' - The name of the domain to delete.
newDeleteDomain ::
  -- | 'domainName'
  Core.Text ->
  DeleteDomain
newDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | The name of the domain to delete.
deleteDomain_domainName :: Lens.Lens' DeleteDomain Core.Text
deleteDomain_domainName = Lens.lens (\DeleteDomain' {domainName} -> domainName) (\s@DeleteDomain' {} a -> s {domainName = a} :: DeleteDomain)

instance Core.AWSRequest DeleteDomain where
  type AWSResponse DeleteDomain = DeleteDomainResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteDomainResponse'

instance Core.Hashable DeleteDomain

instance Core.NFData DeleteDomain

instance Core.ToHeaders DeleteDomain where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteDomain where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDomain where
  toQuery DeleteDomain' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteDomain" :: Core.ByteString),
        "Version" Core.=: ("2009-04-15" :: Core.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDomainResponse ::
  DeleteDomainResponse
newDeleteDomainResponse = DeleteDomainResponse'

instance Core.NFData DeleteDomainResponse
