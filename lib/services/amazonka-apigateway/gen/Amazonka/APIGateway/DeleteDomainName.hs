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
-- Module      : Amazonka.APIGateway.DeleteDomainName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the DomainName resource.
module Amazonka.APIGateway.DeleteDomainName
  ( -- * Creating a Request
    DeleteDomainName (..),
    newDeleteDomainName,

    -- * Request Lenses
    deleteDomainName_domainName,

    -- * Destructuring the Response
    DeleteDomainNameResponse (..),
    newDeleteDomainNameResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete the DomainName resource.
--
-- /See:/ 'newDeleteDomainName' smart constructor.
data DeleteDomainName = DeleteDomainName'
  { -- | The name of the DomainName resource to be deleted.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomainName_domainName' - The name of the DomainName resource to be deleted.
newDeleteDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteDomainName
newDeleteDomainName pDomainName_ =
  DeleteDomainName' {domainName = pDomainName_}

-- | The name of the DomainName resource to be deleted.
deleteDomainName_domainName :: Lens.Lens' DeleteDomainName Prelude.Text
deleteDomainName_domainName = Lens.lens (\DeleteDomainName' {domainName} -> domainName) (\s@DeleteDomainName' {} a -> s {domainName = a} :: DeleteDomainName)

instance Core.AWSRequest DeleteDomainName where
  type
    AWSResponse DeleteDomainName =
      DeleteDomainNameResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteDomainNameResponse'

instance Prelude.Hashable DeleteDomainName where
  hashWithSalt _salt DeleteDomainName' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteDomainName where
  rnf DeleteDomainName' {..} = Prelude.rnf domainName

instance Data.ToHeaders DeleteDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteDomainName where
  toPath DeleteDomainName' {..} =
    Prelude.mconcat
      ["/domainnames/", Data.toBS domainName]

instance Data.ToQuery DeleteDomainName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainNameResponse' smart constructor.
data DeleteDomainNameResponse = DeleteDomainNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDomainNameResponse ::
  DeleteDomainNameResponse
newDeleteDomainNameResponse =
  DeleteDomainNameResponse'

instance Prelude.NFData DeleteDomainNameResponse where
  rnf _ = ()
