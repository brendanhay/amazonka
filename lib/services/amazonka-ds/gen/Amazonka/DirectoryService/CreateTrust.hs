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
-- Module      : Amazonka.DirectoryService.CreateTrust
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Directory Service for Microsoft Active Directory allows you to configure
-- trust relationships. For example, you can establish a trust between your
-- Managed Microsoft AD directory, and your existing self-managed Microsoft
-- Active Directory. This would allow you to provide users and groups
-- access to resources in either domain, with a single set of credentials.
--
-- This action initiates the creation of the Amazon Web Services side of a
-- trust relationship between an Managed Microsoft AD directory and an
-- external domain. You can create either a forest trust or an external
-- trust.
module Amazonka.DirectoryService.CreateTrust
  ( -- * Creating a Request
    CreateTrust (..),
    newCreateTrust,

    -- * Request Lenses
    createTrust_conditionalForwarderIpAddrs,
    createTrust_selectiveAuth,
    createTrust_trustType,
    createTrust_directoryId,
    createTrust_remoteDomainName,
    createTrust_trustPassword,
    createTrust_trustDirection,

    -- * Destructuring the Response
    CreateTrustResponse (..),
    newCreateTrustResponse,

    -- * Response Lenses
    createTrustResponse_trustId,
    createTrustResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Directory Service for Microsoft Active Directory allows you to configure
-- trust relationships. For example, you can establish a trust between your
-- Managed Microsoft AD directory, and your existing self-managed Microsoft
-- Active Directory. This would allow you to provide users and groups
-- access to resources in either domain, with a single set of credentials.
--
-- This action initiates the creation of the Amazon Web Services side of a
-- trust relationship between an Managed Microsoft AD directory and an
-- external domain.
--
-- /See:/ 'newCreateTrust' smart constructor.
data CreateTrust = CreateTrust'
  { -- | The IP addresses of the remote DNS server associated with
    -- RemoteDomainName.
    conditionalForwarderIpAddrs :: Prelude.Maybe [Prelude.Text],
    -- | Optional parameter to enable selective authentication for the trust.
    selectiveAuth :: Prelude.Maybe SelectiveAuth,
    -- | The trust relationship type. @Forest@ is the default.
    trustType :: Prelude.Maybe TrustType,
    -- | The Directory ID of the Managed Microsoft AD directory for which to
    -- establish the trust relationship.
    directoryId :: Prelude.Text,
    -- | The Fully Qualified Domain Name (FQDN) of the external domain for which
    -- to create the trust relationship.
    remoteDomainName :: Prelude.Text,
    -- | The trust password. The must be the same password that was used when
    -- creating the trust relationship on the external domain.
    trustPassword :: Data.Sensitive Prelude.Text,
    -- | The direction of the trust relationship.
    trustDirection :: TrustDirection
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalForwarderIpAddrs', 'createTrust_conditionalForwarderIpAddrs' - The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
--
-- 'selectiveAuth', 'createTrust_selectiveAuth' - Optional parameter to enable selective authentication for the trust.
--
-- 'trustType', 'createTrust_trustType' - The trust relationship type. @Forest@ is the default.
--
-- 'directoryId', 'createTrust_directoryId' - The Directory ID of the Managed Microsoft AD directory for which to
-- establish the trust relationship.
--
-- 'remoteDomainName', 'createTrust_remoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain for which
-- to create the trust relationship.
--
-- 'trustPassword', 'createTrust_trustPassword' - The trust password. The must be the same password that was used when
-- creating the trust relationship on the external domain.
--
-- 'trustDirection', 'createTrust_trustDirection' - The direction of the trust relationship.
newCreateTrust ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'remoteDomainName'
  Prelude.Text ->
  -- | 'trustPassword'
  Prelude.Text ->
  -- | 'trustDirection'
  TrustDirection ->
  CreateTrust
newCreateTrust
  pDirectoryId_
  pRemoteDomainName_
  pTrustPassword_
  pTrustDirection_ =
    CreateTrust'
      { conditionalForwarderIpAddrs =
          Prelude.Nothing,
        selectiveAuth = Prelude.Nothing,
        trustType = Prelude.Nothing,
        directoryId = pDirectoryId_,
        remoteDomainName = pRemoteDomainName_,
        trustPassword =
          Data._Sensitive Lens.# pTrustPassword_,
        trustDirection = pTrustDirection_
      }

-- | The IP addresses of the remote DNS server associated with
-- RemoteDomainName.
createTrust_conditionalForwarderIpAddrs :: Lens.Lens' CreateTrust (Prelude.Maybe [Prelude.Text])
createTrust_conditionalForwarderIpAddrs = Lens.lens (\CreateTrust' {conditionalForwarderIpAddrs} -> conditionalForwarderIpAddrs) (\s@CreateTrust' {} a -> s {conditionalForwarderIpAddrs = a} :: CreateTrust) Prelude.. Lens.mapping Lens.coerced

-- | Optional parameter to enable selective authentication for the trust.
createTrust_selectiveAuth :: Lens.Lens' CreateTrust (Prelude.Maybe SelectiveAuth)
createTrust_selectiveAuth = Lens.lens (\CreateTrust' {selectiveAuth} -> selectiveAuth) (\s@CreateTrust' {} a -> s {selectiveAuth = a} :: CreateTrust)

-- | The trust relationship type. @Forest@ is the default.
createTrust_trustType :: Lens.Lens' CreateTrust (Prelude.Maybe TrustType)
createTrust_trustType = Lens.lens (\CreateTrust' {trustType} -> trustType) (\s@CreateTrust' {} a -> s {trustType = a} :: CreateTrust)

-- | The Directory ID of the Managed Microsoft AD directory for which to
-- establish the trust relationship.
createTrust_directoryId :: Lens.Lens' CreateTrust Prelude.Text
createTrust_directoryId = Lens.lens (\CreateTrust' {directoryId} -> directoryId) (\s@CreateTrust' {} a -> s {directoryId = a} :: CreateTrust)

-- | The Fully Qualified Domain Name (FQDN) of the external domain for which
-- to create the trust relationship.
createTrust_remoteDomainName :: Lens.Lens' CreateTrust Prelude.Text
createTrust_remoteDomainName = Lens.lens (\CreateTrust' {remoteDomainName} -> remoteDomainName) (\s@CreateTrust' {} a -> s {remoteDomainName = a} :: CreateTrust)

-- | The trust password. The must be the same password that was used when
-- creating the trust relationship on the external domain.
createTrust_trustPassword :: Lens.Lens' CreateTrust Prelude.Text
createTrust_trustPassword = Lens.lens (\CreateTrust' {trustPassword} -> trustPassword) (\s@CreateTrust' {} a -> s {trustPassword = a} :: CreateTrust) Prelude.. Data._Sensitive

-- | The direction of the trust relationship.
createTrust_trustDirection :: Lens.Lens' CreateTrust TrustDirection
createTrust_trustDirection = Lens.lens (\CreateTrust' {trustDirection} -> trustDirection) (\s@CreateTrust' {} a -> s {trustDirection = a} :: CreateTrust)

instance Core.AWSRequest CreateTrust where
  type AWSResponse CreateTrust = CreateTrustResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrustResponse'
            Prelude.<$> (x Data..?> "TrustId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrust where
  hashWithSalt _salt CreateTrust' {..} =
    _salt
      `Prelude.hashWithSalt` conditionalForwarderIpAddrs
      `Prelude.hashWithSalt` selectiveAuth
      `Prelude.hashWithSalt` trustType
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` remoteDomainName
      `Prelude.hashWithSalt` trustPassword
      `Prelude.hashWithSalt` trustDirection

instance Prelude.NFData CreateTrust where
  rnf CreateTrust' {..} =
    Prelude.rnf conditionalForwarderIpAddrs
      `Prelude.seq` Prelude.rnf selectiveAuth
      `Prelude.seq` Prelude.rnf trustType
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf remoteDomainName
      `Prelude.seq` Prelude.rnf trustPassword
      `Prelude.seq` Prelude.rnf trustDirection

instance Data.ToHeaders CreateTrust where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateTrust" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTrust where
  toJSON CreateTrust' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalForwarderIpAddrs" Data..=)
              Prelude.<$> conditionalForwarderIpAddrs,
            ("SelectiveAuth" Data..=) Prelude.<$> selectiveAuth,
            ("TrustType" Data..=) Prelude.<$> trustType,
            Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just
              ("RemoteDomainName" Data..= remoteDomainName),
            Prelude.Just ("TrustPassword" Data..= trustPassword),
            Prelude.Just
              ("TrustDirection" Data..= trustDirection)
          ]
      )

instance Data.ToPath CreateTrust where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTrust where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a CreateTrust request.
--
-- /See:/ 'newCreateTrustResponse' smart constructor.
data CreateTrustResponse = CreateTrustResponse'
  { -- | A unique identifier for the trust relationship that was created.
    trustId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrustResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustId', 'createTrustResponse_trustId' - A unique identifier for the trust relationship that was created.
--
-- 'httpStatus', 'createTrustResponse_httpStatus' - The response's http status code.
newCreateTrustResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrustResponse
newCreateTrustResponse pHttpStatus_ =
  CreateTrustResponse'
    { trustId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the trust relationship that was created.
createTrustResponse_trustId :: Lens.Lens' CreateTrustResponse (Prelude.Maybe Prelude.Text)
createTrustResponse_trustId = Lens.lens (\CreateTrustResponse' {trustId} -> trustId) (\s@CreateTrustResponse' {} a -> s {trustId = a} :: CreateTrustResponse)

-- | The response's http status code.
createTrustResponse_httpStatus :: Lens.Lens' CreateTrustResponse Prelude.Int
createTrustResponse_httpStatus = Lens.lens (\CreateTrustResponse' {httpStatus} -> httpStatus) (\s@CreateTrustResponse' {} a -> s {httpStatus = a} :: CreateTrustResponse)

instance Prelude.NFData CreateTrustResponse where
  rnf CreateTrustResponse' {..} =
    Prelude.rnf trustId
      `Prelude.seq` Prelude.rnf httpStatus
