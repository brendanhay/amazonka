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
-- Module      : Network.AWS.Inspector.Types.FailedItemDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FailedItemDetails where

import Network.AWS.Inspector.Types.FailedItemErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Includes details about the failed items.
--
-- /See:/ 'newFailedItemDetails' smart constructor.
data FailedItemDetails = FailedItemDetails'
  { -- | The status code of a failed item.
    failureCode :: FailedItemErrorCode,
    -- | Indicates whether you can immediately retry a request for this item for
    -- a specified resource.
    retryable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedItemDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'failedItemDetails_failureCode' - The status code of a failed item.
--
-- 'retryable', 'failedItemDetails_retryable' - Indicates whether you can immediately retry a request for this item for
-- a specified resource.
newFailedItemDetails ::
  -- | 'failureCode'
  FailedItemErrorCode ->
  -- | 'retryable'
  Prelude.Bool ->
  FailedItemDetails
newFailedItemDetails pFailureCode_ pRetryable_ =
  FailedItemDetails'
    { failureCode = pFailureCode_,
      retryable = pRetryable_
    }

-- | The status code of a failed item.
failedItemDetails_failureCode :: Lens.Lens' FailedItemDetails FailedItemErrorCode
failedItemDetails_failureCode = Lens.lens (\FailedItemDetails' {failureCode} -> failureCode) (\s@FailedItemDetails' {} a -> s {failureCode = a} :: FailedItemDetails)

-- | Indicates whether you can immediately retry a request for this item for
-- a specified resource.
failedItemDetails_retryable :: Lens.Lens' FailedItemDetails Prelude.Bool
failedItemDetails_retryable = Lens.lens (\FailedItemDetails' {retryable} -> retryable) (\s@FailedItemDetails' {} a -> s {retryable = a} :: FailedItemDetails)

instance Prelude.FromJSON FailedItemDetails where
  parseJSON =
    Prelude.withObject
      "FailedItemDetails"
      ( \x ->
          FailedItemDetails'
            Prelude.<$> (x Prelude..: "failureCode")
            Prelude.<*> (x Prelude..: "retryable")
      )

instance Prelude.Hashable FailedItemDetails

instance Prelude.NFData FailedItemDetails
