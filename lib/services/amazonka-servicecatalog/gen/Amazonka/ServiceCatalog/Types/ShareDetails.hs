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
-- Module      : Amazonka.ServiceCatalog.Types.ShareDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ShareDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ShareError

-- | Information about the portfolio share operation.
--
-- /See:/ 'newShareDetails' smart constructor.
data ShareDetails = ShareDetails'
  { -- | List of accounts for whom the operation succeeded.
    successfulShares :: Prelude.Maybe [Prelude.Text],
    -- | List of errors.
    shareErrors :: Prelude.Maybe [ShareError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulShares', 'shareDetails_successfulShares' - List of accounts for whom the operation succeeded.
--
-- 'shareErrors', 'shareDetails_shareErrors' - List of errors.
newShareDetails ::
  ShareDetails
newShareDetails =
  ShareDetails'
    { successfulShares = Prelude.Nothing,
      shareErrors = Prelude.Nothing
    }

-- | List of accounts for whom the operation succeeded.
shareDetails_successfulShares :: Lens.Lens' ShareDetails (Prelude.Maybe [Prelude.Text])
shareDetails_successfulShares = Lens.lens (\ShareDetails' {successfulShares} -> successfulShares) (\s@ShareDetails' {} a -> s {successfulShares = a} :: ShareDetails) Prelude.. Lens.mapping Lens.coerced

-- | List of errors.
shareDetails_shareErrors :: Lens.Lens' ShareDetails (Prelude.Maybe [ShareError])
shareDetails_shareErrors = Lens.lens (\ShareDetails' {shareErrors} -> shareErrors) (\s@ShareDetails' {} a -> s {shareErrors = a} :: ShareDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ShareDetails where
  parseJSON =
    Core.withObject
      "ShareDetails"
      ( \x ->
          ShareDetails'
            Prelude.<$> ( x Core..:? "SuccessfulShares"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ShareErrors" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ShareDetails where
  hashWithSalt _salt ShareDetails' {..} =
    _salt `Prelude.hashWithSalt` successfulShares
      `Prelude.hashWithSalt` shareErrors

instance Prelude.NFData ShareDetails where
  rnf ShareDetails' {..} =
    Prelude.rnf successfulShares
      `Prelude.seq` Prelude.rnf shareErrors
