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
-- Module      : Network.AWS.ApiGatewayV2.DeleteDomainName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain name.
module Network.AWS.ApiGatewayV2.DeleteDomainName
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

import Network.AWS.ApiGatewayV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDomainName' smart constructor.
data DeleteDomainName = DeleteDomainName'
  { -- | The domain name.
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
-- 'domainName', 'deleteDomainName_domainName' - The domain name.
newDeleteDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteDomainName
newDeleteDomainName pDomainName_ =
  DeleteDomainName' {domainName = pDomainName_}

-- | The domain name.
deleteDomainName_domainName :: Lens.Lens' DeleteDomainName Prelude.Text
deleteDomainName_domainName = Lens.lens (\DeleteDomainName' {domainName} -> domainName) (\s@DeleteDomainName' {} a -> s {domainName = a} :: DeleteDomainName)

instance Core.AWSRequest DeleteDomainName where
  type
    AWSResponse DeleteDomainName =
      DeleteDomainNameResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteDomainNameResponse'

instance Prelude.Hashable DeleteDomainName

instance Prelude.NFData DeleteDomainName

instance Core.ToHeaders DeleteDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteDomainName where
  toPath DeleteDomainName' {..} =
    Prelude.mconcat
      ["/v2/domainnames/", Core.toBS domainName]

instance Core.ToQuery DeleteDomainName where
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

instance Prelude.NFData DeleteDomainNameResponse
