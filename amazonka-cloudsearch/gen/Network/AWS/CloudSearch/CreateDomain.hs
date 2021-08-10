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
-- Module      : Network.AWS.CloudSearch.CreateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new search domain. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/creating-domains.html Creating a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_domainName,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @CreateDomain@ operation. Specifies
-- a name for the new search domain.
--
-- /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | A name for the domain you are creating. Allowed characters are a-z
    -- (lower-case letters), 0-9, and hyphen (-). Domain names must start with
    -- a letter or number and be at least 3 and no more than 28 characters
    -- long.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'createDomain_domainName' - A name for the domain you are creating. Allowed characters are a-z
-- (lower-case letters), 0-9, and hyphen (-). Domain names must start with
-- a letter or number and be at least 3 and no more than 28 characters
-- long.
newCreateDomain ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomain
newCreateDomain pDomainName_ =
  CreateDomain' {domainName = pDomainName_}

-- | A name for the domain you are creating. Allowed characters are a-z
-- (lower-case letters), 0-9, and hyphen (-). Domain names must start with
-- a letter or number and be at least 3 and no more than 28 characters
-- long.
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDomainResult"
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Core..@? "DomainStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain

instance Prelude.NFData CreateDomain

instance Core.ToHeaders CreateDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDomain where
  toQuery CreateDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateDomain" :: Prelude.ByteString),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @CreateDomainRequest@. Contains the status of a newly
-- created domain.
--
-- /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { domainStatus :: Prelude.Maybe DomainStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'createDomainResponse_domainStatus' - Undocumented member.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainResponse
newCreateDomainResponse pHttpStatus_ =
  CreateDomainResponse'
    { domainStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDomainResponse_domainStatus :: Lens.Lens' CreateDomainResponse (Prelude.Maybe DomainStatus)
createDomainResponse_domainStatus = Lens.lens (\CreateDomainResponse' {domainStatus} -> domainStatus) (\s@CreateDomainResponse' {} a -> s {domainStatus = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

instance Prelude.NFData CreateDomainResponse
