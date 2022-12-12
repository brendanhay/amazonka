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
-- Module      : Amazonka.RolesAnywhere.ImportCrl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the certificate revocation list (CRL). CRl is a list of
-- certificates that have been revoked by the issuing certificate Authority
-- (CA). IAM Roles Anywhere validates against the crl list before issuing
-- credentials.
--
-- __Required permissions:__ @rolesanywhere:ImportCrl@.
module Amazonka.RolesAnywhere.ImportCrl
  ( -- * Creating a Request
    ImportCrl (..),
    newImportCrl,

    -- * Request Lenses
    importCrl_enabled,
    importCrl_tags,
    importCrl_crlData,
    importCrl_name,
    importCrl_trustAnchorArn,

    -- * Destructuring the Response
    CrlDetailResponse (..),
    newCrlDetailResponse,

    -- * Response Lenses
    crlDetailResponse_crl,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newImportCrl' smart constructor.
data ImportCrl = ImportCrl'
  { -- | Specifies whether the certificate revocation list (CRL) is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of tags to attach to the certificate revocation list (CRL).
    tags :: Prelude.Maybe [Tag],
    -- | The x509 v3 specified certificate revocation list
    crlData :: Data.Base64,
    -- | The name of the certificate revocation list (CRL).
    name :: Prelude.Text,
    -- | The ARN of the TrustAnchor the certificate revocation list (CRL) will
    -- provide revocation for.
    trustAnchorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'importCrl_enabled' - Specifies whether the certificate revocation list (CRL) is enabled.
--
-- 'tags', 'importCrl_tags' - A list of tags to attach to the certificate revocation list (CRL).
--
-- 'crlData', 'importCrl_crlData' - The x509 v3 specified certificate revocation list--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'name', 'importCrl_name' - The name of the certificate revocation list (CRL).
--
-- 'trustAnchorArn', 'importCrl_trustAnchorArn' - The ARN of the TrustAnchor the certificate revocation list (CRL) will
-- provide revocation for.
newImportCrl ::
  -- | 'crlData'
  Prelude.ByteString ->
  -- | 'name'
  Prelude.Text ->
  -- | 'trustAnchorArn'
  Prelude.Text ->
  ImportCrl
newImportCrl pCrlData_ pName_ pTrustAnchorArn_ =
  ImportCrl'
    { enabled = Prelude.Nothing,
      tags = Prelude.Nothing,
      crlData = Data._Base64 Lens.# pCrlData_,
      name = pName_,
      trustAnchorArn = pTrustAnchorArn_
    }

-- | Specifies whether the certificate revocation list (CRL) is enabled.
importCrl_enabled :: Lens.Lens' ImportCrl (Prelude.Maybe Prelude.Bool)
importCrl_enabled = Lens.lens (\ImportCrl' {enabled} -> enabled) (\s@ImportCrl' {} a -> s {enabled = a} :: ImportCrl)

-- | A list of tags to attach to the certificate revocation list (CRL).
importCrl_tags :: Lens.Lens' ImportCrl (Prelude.Maybe [Tag])
importCrl_tags = Lens.lens (\ImportCrl' {tags} -> tags) (\s@ImportCrl' {} a -> s {tags = a} :: ImportCrl) Prelude.. Lens.mapping Lens.coerced

-- | The x509 v3 specified certificate revocation list--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCrl_crlData :: Lens.Lens' ImportCrl Prelude.ByteString
importCrl_crlData = Lens.lens (\ImportCrl' {crlData} -> crlData) (\s@ImportCrl' {} a -> s {crlData = a} :: ImportCrl) Prelude.. Data._Base64

-- | The name of the certificate revocation list (CRL).
importCrl_name :: Lens.Lens' ImportCrl Prelude.Text
importCrl_name = Lens.lens (\ImportCrl' {name} -> name) (\s@ImportCrl' {} a -> s {name = a} :: ImportCrl)

-- | The ARN of the TrustAnchor the certificate revocation list (CRL) will
-- provide revocation for.
importCrl_trustAnchorArn :: Lens.Lens' ImportCrl Prelude.Text
importCrl_trustAnchorArn = Lens.lens (\ImportCrl' {trustAnchorArn} -> trustAnchorArn) (\s@ImportCrl' {} a -> s {trustAnchorArn = a} :: ImportCrl)

instance Core.AWSRequest ImportCrl where
  type AWSResponse ImportCrl = CrlDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ImportCrl where
  hashWithSalt _salt ImportCrl' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` crlData
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` trustAnchorArn

instance Prelude.NFData ImportCrl where
  rnf ImportCrl' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf crlData
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf trustAnchorArn

instance Data.ToHeaders ImportCrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportCrl where
  toJSON ImportCrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("crlData" Data..= crlData),
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("trustAnchorArn" Data..= trustAnchorArn)
          ]
      )

instance Data.ToPath ImportCrl where
  toPath = Prelude.const "/crls"

instance Data.ToQuery ImportCrl where
  toQuery = Prelude.const Prelude.mempty
