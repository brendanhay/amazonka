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
-- Module      : Amazonka.WorkSpacesWeb.Types.TrustStoreSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.TrustStoreSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of the trust store.
--
-- /See:/ 'newTrustStoreSummary' smart constructor.
data TrustStoreSummary = TrustStoreSummary'
  { -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustStoreSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustStoreArn', 'trustStoreSummary_trustStoreArn' - The ARN of the trust store.
newTrustStoreSummary ::
  TrustStoreSummary
newTrustStoreSummary =
  TrustStoreSummary' {trustStoreArn = Prelude.Nothing}

-- | The ARN of the trust store.
trustStoreSummary_trustStoreArn :: Lens.Lens' TrustStoreSummary (Prelude.Maybe Prelude.Text)
trustStoreSummary_trustStoreArn = Lens.lens (\TrustStoreSummary' {trustStoreArn} -> trustStoreArn) (\s@TrustStoreSummary' {} a -> s {trustStoreArn = a} :: TrustStoreSummary)

instance Core.FromJSON TrustStoreSummary where
  parseJSON =
    Core.withObject
      "TrustStoreSummary"
      ( \x ->
          TrustStoreSummary'
            Prelude.<$> (x Core..:? "trustStoreArn")
      )

instance Prelude.Hashable TrustStoreSummary where
  hashWithSalt _salt TrustStoreSummary' {..} =
    _salt `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData TrustStoreSummary where
  rnf TrustStoreSummary' {..} =
    Prelude.rnf trustStoreArn
