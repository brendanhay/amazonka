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
-- Module      : Amazonka.RolesAnywhere.Types.TrustAnchorDetailResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.TrustAnchorDetailResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.TrustAnchorDetail

-- | /See:/ 'newTrustAnchorDetailResponse' smart constructor.
data TrustAnchorDetailResponse = TrustAnchorDetailResponse'
  { -- | The state of the trust anchor after a read or write operation.
    trustAnchor :: TrustAnchorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustAnchorDetailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustAnchor', 'trustAnchorDetailResponse_trustAnchor' - The state of the trust anchor after a read or write operation.
newTrustAnchorDetailResponse ::
  -- | 'trustAnchor'
  TrustAnchorDetail ->
  TrustAnchorDetailResponse
newTrustAnchorDetailResponse pTrustAnchor_ =
  TrustAnchorDetailResponse'
    { trustAnchor =
        pTrustAnchor_
    }

-- | The state of the trust anchor after a read or write operation.
trustAnchorDetailResponse_trustAnchor :: Lens.Lens' TrustAnchorDetailResponse TrustAnchorDetail
trustAnchorDetailResponse_trustAnchor = Lens.lens (\TrustAnchorDetailResponse' {trustAnchor} -> trustAnchor) (\s@TrustAnchorDetailResponse' {} a -> s {trustAnchor = a} :: TrustAnchorDetailResponse)

instance Data.FromJSON TrustAnchorDetailResponse where
  parseJSON =
    Data.withObject
      "TrustAnchorDetailResponse"
      ( \x ->
          TrustAnchorDetailResponse'
            Prelude.<$> (x Data..: "trustAnchor")
      )

instance Prelude.Hashable TrustAnchorDetailResponse where
  hashWithSalt _salt TrustAnchorDetailResponse' {..} =
    _salt `Prelude.hashWithSalt` trustAnchor

instance Prelude.NFData TrustAnchorDetailResponse where
  rnf TrustAnchorDetailResponse' {..} =
    Prelude.rnf trustAnchor
