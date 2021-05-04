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
-- Module      : Network.AWS.SDB.CreateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateDomain@ operation creates a new domain. The domain name
-- should be unique among the domains associated with the Access Key ID
-- provided in the request. The @CreateDomain@ operation may take 10 or
-- more seconds to complete.
--
-- The client can create up to 100 domains per account.
--
-- If the client requires additional domains, go to
-- <http://aws.amazon.com/contact-us/simpledb-limit-request/>.
module Network.AWS.SDB.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_domainName,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | The name of the domain to create. The name can range between 3 and 255
    -- characters and can contain the following characters: a-z, A-Z, 0-9,
    -- \'_\', \'-\', and \'.\'.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'createDomain_domainName' - The name of the domain to create. The name can range between 3 and 255
-- characters and can contain the following characters: a-z, A-Z, 0-9,
-- \'_\', \'-\', and \'.\'.
newCreateDomain ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomain
newCreateDomain pDomainName_ =
  CreateDomain' {domainName = pDomainName_}

-- | The name of the domain to create. The name can range between 3 and 255
-- characters and can contain the following characters: a-z, A-Z, 0-9,
-- \'_\', \'-\', and \'.\'.
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

instance Prelude.AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull CreateDomainResponse'

instance Prelude.Hashable CreateDomain

instance Prelude.NFData CreateDomain

instance Prelude.ToHeaders CreateDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDomain where
  toQuery CreateDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateDomain" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2009-04-15" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName
      ]

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateDomainResponse ::
  CreateDomainResponse
newCreateDomainResponse = CreateDomainResponse'

instance Prelude.NFData CreateDomainResponse
