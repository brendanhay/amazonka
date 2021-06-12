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
-- Module      : Network.AWS.ServiceCatalog.Types.ShareDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ShareError

-- | Information about the portfolio share operation.
--
-- /See:/ 'newShareDetails' smart constructor.
data ShareDetails = ShareDetails'
  { -- | List of errors.
    shareErrors :: Core.Maybe [ShareError],
    -- | List of accounts for whom the operation succeeded.
    successfulShares :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ShareDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareErrors', 'shareDetails_shareErrors' - List of errors.
--
-- 'successfulShares', 'shareDetails_successfulShares' - List of accounts for whom the operation succeeded.
newShareDetails ::
  ShareDetails
newShareDetails =
  ShareDetails'
    { shareErrors = Core.Nothing,
      successfulShares = Core.Nothing
    }

-- | List of errors.
shareDetails_shareErrors :: Lens.Lens' ShareDetails (Core.Maybe [ShareError])
shareDetails_shareErrors = Lens.lens (\ShareDetails' {shareErrors} -> shareErrors) (\s@ShareDetails' {} a -> s {shareErrors = a} :: ShareDetails) Core.. Lens.mapping Lens._Coerce

-- | List of accounts for whom the operation succeeded.
shareDetails_successfulShares :: Lens.Lens' ShareDetails (Core.Maybe [Core.Text])
shareDetails_successfulShares = Lens.lens (\ShareDetails' {successfulShares} -> successfulShares) (\s@ShareDetails' {} a -> s {successfulShares = a} :: ShareDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ShareDetails where
  parseJSON =
    Core.withObject
      "ShareDetails"
      ( \x ->
          ShareDetails'
            Core.<$> (x Core..:? "ShareErrors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SuccessfulShares" Core..!= Core.mempty)
      )

instance Core.Hashable ShareDetails

instance Core.NFData ShareDetails
