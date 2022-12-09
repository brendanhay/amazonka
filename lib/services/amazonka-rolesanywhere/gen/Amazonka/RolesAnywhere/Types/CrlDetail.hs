{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RolesAnywhere.Types.CrlDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.CrlDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The state of the certificate revocation list (CRL) after a read or write
-- operation.
--
-- /See:/ 'newCrlDetail' smart constructor.
data CrlDetail = CrlDetail'
  { -- | The ISO-8601 timestamp when the certificate revocation list (CRL) was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the certificate revocation list (CRL).
    crlArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the certificate revocation list (CRL) after a read or write
    -- operation.
    crlData :: Prelude.Maybe Data.Base64,
    -- | The unique identifier of the certificate revocation list (CRL).
    crlId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the certificate revocation list (CRL) is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the certificate revocation list (CRL).
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the TrustAnchor the certificate revocation list (CRL) will
    -- provide revocation for.
    trustAnchorArn :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 timestamp when the certificate revocation list (CRL) was
    -- last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrlDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'crlDetail_createdAt' - The ISO-8601 timestamp when the certificate revocation list (CRL) was
-- created.
--
-- 'crlArn', 'crlDetail_crlArn' - The ARN of the certificate revocation list (CRL).
--
-- 'crlData', 'crlDetail_crlData' - The state of the certificate revocation list (CRL) after a read or write
-- operation.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'crlId', 'crlDetail_crlId' - The unique identifier of the certificate revocation list (CRL).
--
-- 'enabled', 'crlDetail_enabled' - Indicates whether the certificate revocation list (CRL) is enabled.
--
-- 'name', 'crlDetail_name' - The name of the certificate revocation list (CRL).
--
-- 'trustAnchorArn', 'crlDetail_trustAnchorArn' - The ARN of the TrustAnchor the certificate revocation list (CRL) will
-- provide revocation for.
--
-- 'updatedAt', 'crlDetail_updatedAt' - The ISO-8601 timestamp when the certificate revocation list (CRL) was
-- last updated.
newCrlDetail ::
  CrlDetail
newCrlDetail =
  CrlDetail'
    { createdAt = Prelude.Nothing,
      crlArn = Prelude.Nothing,
      crlData = Prelude.Nothing,
      crlId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      name = Prelude.Nothing,
      trustAnchorArn = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The ISO-8601 timestamp when the certificate revocation list (CRL) was
-- created.
crlDetail_createdAt :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.UTCTime)
crlDetail_createdAt = Lens.lens (\CrlDetail' {createdAt} -> createdAt) (\s@CrlDetail' {} a -> s {createdAt = a} :: CrlDetail) Prelude.. Lens.mapping Data._Time

-- | The ARN of the certificate revocation list (CRL).
crlDetail_crlArn :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.Text)
crlDetail_crlArn = Lens.lens (\CrlDetail' {crlArn} -> crlArn) (\s@CrlDetail' {} a -> s {crlArn = a} :: CrlDetail)

-- | The state of the certificate revocation list (CRL) after a read or write
-- operation.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
crlDetail_crlData :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.ByteString)
crlDetail_crlData = Lens.lens (\CrlDetail' {crlData} -> crlData) (\s@CrlDetail' {} a -> s {crlData = a} :: CrlDetail) Prelude.. Lens.mapping Data._Base64

-- | The unique identifier of the certificate revocation list (CRL).
crlDetail_crlId :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.Text)
crlDetail_crlId = Lens.lens (\CrlDetail' {crlId} -> crlId) (\s@CrlDetail' {} a -> s {crlId = a} :: CrlDetail)

-- | Indicates whether the certificate revocation list (CRL) is enabled.
crlDetail_enabled :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.Bool)
crlDetail_enabled = Lens.lens (\CrlDetail' {enabled} -> enabled) (\s@CrlDetail' {} a -> s {enabled = a} :: CrlDetail)

-- | The name of the certificate revocation list (CRL).
crlDetail_name :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.Text)
crlDetail_name = Lens.lens (\CrlDetail' {name} -> name) (\s@CrlDetail' {} a -> s {name = a} :: CrlDetail)

-- | The ARN of the TrustAnchor the certificate revocation list (CRL) will
-- provide revocation for.
crlDetail_trustAnchorArn :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.Text)
crlDetail_trustAnchorArn = Lens.lens (\CrlDetail' {trustAnchorArn} -> trustAnchorArn) (\s@CrlDetail' {} a -> s {trustAnchorArn = a} :: CrlDetail)

-- | The ISO-8601 timestamp when the certificate revocation list (CRL) was
-- last updated.
crlDetail_updatedAt :: Lens.Lens' CrlDetail (Prelude.Maybe Prelude.UTCTime)
crlDetail_updatedAt = Lens.lens (\CrlDetail' {updatedAt} -> updatedAt) (\s@CrlDetail' {} a -> s {updatedAt = a} :: CrlDetail) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CrlDetail where
  parseJSON =
    Data.withObject
      "CrlDetail"
      ( \x ->
          CrlDetail'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "crlArn")
            Prelude.<*> (x Data..:? "crlData")
            Prelude.<*> (x Data..:? "crlId")
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "trustAnchorArn")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable CrlDetail where
  hashWithSalt _salt CrlDetail' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` crlArn
      `Prelude.hashWithSalt` crlData
      `Prelude.hashWithSalt` crlId
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` trustAnchorArn
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData CrlDetail where
  rnf CrlDetail' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf crlArn
      `Prelude.seq` Prelude.rnf crlData
      `Prelude.seq` Prelude.rnf crlId
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf trustAnchorArn
      `Prelude.seq` Prelude.rnf updatedAt
