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
-- Module      : Amazonka.RolesAnywhere.Types.CrlDetailResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.CrlDetailResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.CrlDetail

-- | /See:/ 'newCrlDetailResponse' smart constructor.
data CrlDetailResponse = CrlDetailResponse'
  { -- | The state of the certificate revocation list (CRL) after a read or write
    -- operation.
    crl :: CrlDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrlDetailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crl', 'crlDetailResponse_crl' - The state of the certificate revocation list (CRL) after a read or write
-- operation.
newCrlDetailResponse ::
  -- | 'crl'
  CrlDetail ->
  CrlDetailResponse
newCrlDetailResponse pCrl_ =
  CrlDetailResponse' {crl = pCrl_}

-- | The state of the certificate revocation list (CRL) after a read or write
-- operation.
crlDetailResponse_crl :: Lens.Lens' CrlDetailResponse CrlDetail
crlDetailResponse_crl = Lens.lens (\CrlDetailResponse' {crl} -> crl) (\s@CrlDetailResponse' {} a -> s {crl = a} :: CrlDetailResponse)

instance Data.FromJSON CrlDetailResponse where
  parseJSON =
    Data.withObject
      "CrlDetailResponse"
      ( \x ->
          CrlDetailResponse' Prelude.<$> (x Data..: "crl")
      )

instance Prelude.Hashable CrlDetailResponse where
  hashWithSalt _salt CrlDetailResponse' {..} =
    _salt `Prelude.hashWithSalt` crl

instance Prelude.NFData CrlDetailResponse where
  rnf CrlDetailResponse' {..} = Prelude.rnf crl
