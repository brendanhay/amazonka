{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ShareError

-- | Information about the portfolio share operation.
--
-- /See:/ 'newShareDetails' smart constructor.
data ShareDetails = ShareDetails'
  { -- | List of errors.
    shareErrors :: Prelude.Maybe [ShareError],
    -- | List of accounts for whom the operation succeeded.
    successfulShares :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { shareErrors = Prelude.Nothing,
      successfulShares = Prelude.Nothing
    }

-- | List of errors.
shareDetails_shareErrors :: Lens.Lens' ShareDetails (Prelude.Maybe [ShareError])
shareDetails_shareErrors = Lens.lens (\ShareDetails' {shareErrors} -> shareErrors) (\s@ShareDetails' {} a -> s {shareErrors = a} :: ShareDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | List of accounts for whom the operation succeeded.
shareDetails_successfulShares :: Lens.Lens' ShareDetails (Prelude.Maybe [Prelude.Text])
shareDetails_successfulShares = Lens.lens (\ShareDetails' {successfulShares} -> successfulShares) (\s@ShareDetails' {} a -> s {successfulShares = a} :: ShareDetails) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ShareDetails where
  parseJSON =
    Prelude.withObject
      "ShareDetails"
      ( \x ->
          ShareDetails'
            Prelude.<$> ( x Prelude..:? "ShareErrors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "SuccessfulShares"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ShareDetails

instance Prelude.NFData ShareDetails
