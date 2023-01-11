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
-- Module      : Amazonka.KMS.Types.XksProxyAuthenticationCredentialType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.XksProxyAuthenticationCredentialType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | KMS uses the authentication credential to sign requests that it sends to
-- the external key store proxy (XKS proxy) on your behalf. You establish
-- these credentials on your external key store proxy and report them to
-- KMS.
--
-- The @XksProxyAuthenticationCredential@ includes two required elements.
--
-- /See:/ 'newXksProxyAuthenticationCredentialType' smart constructor.
data XksProxyAuthenticationCredentialType = XksProxyAuthenticationCredentialType'
  { -- | A unique identifier for the raw secret access key.
    accessKeyId :: Data.Sensitive Prelude.Text,
    -- | A secret string of 43-64 characters. Valid characters are a-z, A-Z, 0-9,
    -- \/, +, and =.
    rawSecretAccessKey :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XksProxyAuthenticationCredentialType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'xksProxyAuthenticationCredentialType_accessKeyId' - A unique identifier for the raw secret access key.
--
-- 'rawSecretAccessKey', 'xksProxyAuthenticationCredentialType_rawSecretAccessKey' - A secret string of 43-64 characters. Valid characters are a-z, A-Z, 0-9,
-- \/, +, and =.
newXksProxyAuthenticationCredentialType ::
  -- | 'accessKeyId'
  Prelude.Text ->
  -- | 'rawSecretAccessKey'
  Prelude.Text ->
  XksProxyAuthenticationCredentialType
newXksProxyAuthenticationCredentialType
  pAccessKeyId_
  pRawSecretAccessKey_ =
    XksProxyAuthenticationCredentialType'
      { accessKeyId =
          Data._Sensitive
            Lens.# pAccessKeyId_,
        rawSecretAccessKey =
          Data._Sensitive
            Lens.# pRawSecretAccessKey_
      }

-- | A unique identifier for the raw secret access key.
xksProxyAuthenticationCredentialType_accessKeyId :: Lens.Lens' XksProxyAuthenticationCredentialType Prelude.Text
xksProxyAuthenticationCredentialType_accessKeyId = Lens.lens (\XksProxyAuthenticationCredentialType' {accessKeyId} -> accessKeyId) (\s@XksProxyAuthenticationCredentialType' {} a -> s {accessKeyId = a} :: XksProxyAuthenticationCredentialType) Prelude.. Data._Sensitive

-- | A secret string of 43-64 characters. Valid characters are a-z, A-Z, 0-9,
-- \/, +, and =.
xksProxyAuthenticationCredentialType_rawSecretAccessKey :: Lens.Lens' XksProxyAuthenticationCredentialType Prelude.Text
xksProxyAuthenticationCredentialType_rawSecretAccessKey = Lens.lens (\XksProxyAuthenticationCredentialType' {rawSecretAccessKey} -> rawSecretAccessKey) (\s@XksProxyAuthenticationCredentialType' {} a -> s {rawSecretAccessKey = a} :: XksProxyAuthenticationCredentialType) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    XksProxyAuthenticationCredentialType
  where
  hashWithSalt
    _salt
    XksProxyAuthenticationCredentialType' {..} =
      _salt `Prelude.hashWithSalt` accessKeyId
        `Prelude.hashWithSalt` rawSecretAccessKey

instance
  Prelude.NFData
    XksProxyAuthenticationCredentialType
  where
  rnf XksProxyAuthenticationCredentialType' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf rawSecretAccessKey

instance
  Data.ToJSON
    XksProxyAuthenticationCredentialType
  where
  toJSON XksProxyAuthenticationCredentialType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccessKeyId" Data..= accessKeyId),
            Prelude.Just
              ("RawSecretAccessKey" Data..= rawSecretAccessKey)
          ]
      )
