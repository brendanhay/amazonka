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
-- Module      : Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for ARPC response generation using method1
-- after ARQC verification is successful.
--
-- /See:/ 'newCryptogramVerificationArpcMethod1' smart constructor.
data CryptogramVerificationArpcMethod1 = CryptogramVerificationArpcMethod1'
  { -- | The auth code used to calculate APRC after ARQC verification is
    -- successful. This is the same auth code used for ARQC generation outside
    -- of Amazon Web Services Payment Cryptography.
    authResponseCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CryptogramVerificationArpcMethod1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authResponseCode', 'cryptogramVerificationArpcMethod1_authResponseCode' - The auth code used to calculate APRC after ARQC verification is
-- successful. This is the same auth code used for ARQC generation outside
-- of Amazon Web Services Payment Cryptography.
newCryptogramVerificationArpcMethod1 ::
  -- | 'authResponseCode'
  Prelude.Text ->
  CryptogramVerificationArpcMethod1
newCryptogramVerificationArpcMethod1
  pAuthResponseCode_ =
    CryptogramVerificationArpcMethod1'
      { authResponseCode =
          pAuthResponseCode_
      }

-- | The auth code used to calculate APRC after ARQC verification is
-- successful. This is the same auth code used for ARQC generation outside
-- of Amazon Web Services Payment Cryptography.
cryptogramVerificationArpcMethod1_authResponseCode :: Lens.Lens' CryptogramVerificationArpcMethod1 Prelude.Text
cryptogramVerificationArpcMethod1_authResponseCode = Lens.lens (\CryptogramVerificationArpcMethod1' {authResponseCode} -> authResponseCode) (\s@CryptogramVerificationArpcMethod1' {} a -> s {authResponseCode = a} :: CryptogramVerificationArpcMethod1)

instance
  Prelude.Hashable
    CryptogramVerificationArpcMethod1
  where
  hashWithSalt
    _salt
    CryptogramVerificationArpcMethod1' {..} =
      _salt `Prelude.hashWithSalt` authResponseCode

instance
  Prelude.NFData
    CryptogramVerificationArpcMethod1
  where
  rnf CryptogramVerificationArpcMethod1' {..} =
    Prelude.rnf authResponseCode

instance
  Data.ToJSON
    CryptogramVerificationArpcMethod1
  where
  toJSON CryptogramVerificationArpcMethod1' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AuthResponseCode" Data..= authResponseCode)
          ]
      )
