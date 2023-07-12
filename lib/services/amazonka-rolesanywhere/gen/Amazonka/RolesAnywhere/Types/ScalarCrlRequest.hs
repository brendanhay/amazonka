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
-- Module      : Amazonka.RolesAnywhere.Types.ScalarCrlRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.ScalarCrlRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newScalarCrlRequest' smart constructor.
data ScalarCrlRequest = ScalarCrlRequest'
  { -- | The unique identifier of the certificate revocation list (CRL).
    crlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalarCrlRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crlId', 'scalarCrlRequest_crlId' - The unique identifier of the certificate revocation list (CRL).
newScalarCrlRequest ::
  -- | 'crlId'
  Prelude.Text ->
  ScalarCrlRequest
newScalarCrlRequest pCrlId_ =
  ScalarCrlRequest' {crlId = pCrlId_}

-- | The unique identifier of the certificate revocation list (CRL).
scalarCrlRequest_crlId :: Lens.Lens' ScalarCrlRequest Prelude.Text
scalarCrlRequest_crlId = Lens.lens (\ScalarCrlRequest' {crlId} -> crlId) (\s@ScalarCrlRequest' {} a -> s {crlId = a} :: ScalarCrlRequest)

instance Prelude.Hashable ScalarCrlRequest where
  hashWithSalt _salt ScalarCrlRequest' {..} =
    _salt `Prelude.hashWithSalt` crlId

instance Prelude.NFData ScalarCrlRequest where
  rnf ScalarCrlRequest' {..} = Prelude.rnf crlId

instance Data.ToJSON ScalarCrlRequest where
  toJSON ScalarCrlRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("crlId" Data..= crlId)]
      )
