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
-- Module      : Amazonka.RolesAnywhere.Types.ScalarTrustAnchorRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.ScalarTrustAnchorRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newScalarTrustAnchorRequest' smart constructor.
data ScalarTrustAnchorRequest = ScalarTrustAnchorRequest'
  { -- | The unique identifier of the trust anchor.
    trustAnchorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalarTrustAnchorRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustAnchorId', 'scalarTrustAnchorRequest_trustAnchorId' - The unique identifier of the trust anchor.
newScalarTrustAnchorRequest ::
  -- | 'trustAnchorId'
  Prelude.Text ->
  ScalarTrustAnchorRequest
newScalarTrustAnchorRequest pTrustAnchorId_ =
  ScalarTrustAnchorRequest'
    { trustAnchorId =
        pTrustAnchorId_
    }

-- | The unique identifier of the trust anchor.
scalarTrustAnchorRequest_trustAnchorId :: Lens.Lens' ScalarTrustAnchorRequest Prelude.Text
scalarTrustAnchorRequest_trustAnchorId = Lens.lens (\ScalarTrustAnchorRequest' {trustAnchorId} -> trustAnchorId) (\s@ScalarTrustAnchorRequest' {} a -> s {trustAnchorId = a} :: ScalarTrustAnchorRequest)

instance Prelude.Hashable ScalarTrustAnchorRequest where
  hashWithSalt _salt ScalarTrustAnchorRequest' {..} =
    _salt `Prelude.hashWithSalt` trustAnchorId

instance Prelude.NFData ScalarTrustAnchorRequest where
  rnf ScalarTrustAnchorRequest' {..} =
    Prelude.rnf trustAnchorId

instance Data.ToJSON ScalarTrustAnchorRequest where
  toJSON ScalarTrustAnchorRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("trustAnchorId" Data..= trustAnchorId)
          ]
      )
