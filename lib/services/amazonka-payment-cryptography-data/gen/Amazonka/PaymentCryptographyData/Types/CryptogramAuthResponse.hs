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
-- Module      : Amazonka.PaymentCryptographyData.Types.CryptogramAuthResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.CryptogramAuthResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod1
import Amazonka.PaymentCryptographyData.Types.CryptogramVerificationArpcMethod2
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for Authorization Response Cryptogram
-- (ARPC) generation after Authorization Request Cryptogram (ARQC)
-- verification is successful.
--
-- /See:/ 'newCryptogramAuthResponse' smart constructor.
data CryptogramAuthResponse = CryptogramAuthResponse'
  { -- | Parameters that are required for ARPC response generation using method1
    -- after ARQC verification is successful.
    arpcMethod1 :: Prelude.Maybe CryptogramVerificationArpcMethod1,
    -- | Parameters that are required for ARPC response generation using method2
    -- after ARQC verification is successful.
    arpcMethod2 :: Prelude.Maybe CryptogramVerificationArpcMethod2
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CryptogramAuthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arpcMethod1', 'cryptogramAuthResponse_arpcMethod1' - Parameters that are required for ARPC response generation using method1
-- after ARQC verification is successful.
--
-- 'arpcMethod2', 'cryptogramAuthResponse_arpcMethod2' - Parameters that are required for ARPC response generation using method2
-- after ARQC verification is successful.
newCryptogramAuthResponse ::
  CryptogramAuthResponse
newCryptogramAuthResponse =
  CryptogramAuthResponse'
    { arpcMethod1 =
        Prelude.Nothing,
      arpcMethod2 = Prelude.Nothing
    }

-- | Parameters that are required for ARPC response generation using method1
-- after ARQC verification is successful.
cryptogramAuthResponse_arpcMethod1 :: Lens.Lens' CryptogramAuthResponse (Prelude.Maybe CryptogramVerificationArpcMethod1)
cryptogramAuthResponse_arpcMethod1 = Lens.lens (\CryptogramAuthResponse' {arpcMethod1} -> arpcMethod1) (\s@CryptogramAuthResponse' {} a -> s {arpcMethod1 = a} :: CryptogramAuthResponse)

-- | Parameters that are required for ARPC response generation using method2
-- after ARQC verification is successful.
cryptogramAuthResponse_arpcMethod2 :: Lens.Lens' CryptogramAuthResponse (Prelude.Maybe CryptogramVerificationArpcMethod2)
cryptogramAuthResponse_arpcMethod2 = Lens.lens (\CryptogramAuthResponse' {arpcMethod2} -> arpcMethod2) (\s@CryptogramAuthResponse' {} a -> s {arpcMethod2 = a} :: CryptogramAuthResponse)

instance Prelude.Hashable CryptogramAuthResponse where
  hashWithSalt _salt CryptogramAuthResponse' {..} =
    _salt
      `Prelude.hashWithSalt` arpcMethod1
      `Prelude.hashWithSalt` arpcMethod2

instance Prelude.NFData CryptogramAuthResponse where
  rnf CryptogramAuthResponse' {..} =
    Prelude.rnf arpcMethod1
      `Prelude.seq` Prelude.rnf arpcMethod2

instance Data.ToJSON CryptogramAuthResponse where
  toJSON CryptogramAuthResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArpcMethod1" Data..=) Prelude.<$> arpcMethod1,
            ("ArpcMethod2" Data..=) Prelude.<$> arpcMethod2
          ]
      )
