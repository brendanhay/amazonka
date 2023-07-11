{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LakeFormation.Types.TransactionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.TransactionStatus
  ( TransactionStatus
      ( ..,
        TransactionStatus_ABORTED,
        TransactionStatus_ACTIVE,
        TransactionStatus_COMMITTED,
        TransactionStatus_COMMIT_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransactionStatus = TransactionStatus'
  { fromTransactionStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern TransactionStatus_ABORTED :: TransactionStatus
pattern TransactionStatus_ABORTED = TransactionStatus' "ABORTED"

pattern TransactionStatus_ACTIVE :: TransactionStatus
pattern TransactionStatus_ACTIVE = TransactionStatus' "ACTIVE"

pattern TransactionStatus_COMMITTED :: TransactionStatus
pattern TransactionStatus_COMMITTED = TransactionStatus' "COMMITTED"

pattern TransactionStatus_COMMIT_IN_PROGRESS :: TransactionStatus
pattern TransactionStatus_COMMIT_IN_PROGRESS = TransactionStatus' "COMMIT_IN_PROGRESS"

{-# COMPLETE
  TransactionStatus_ABORTED,
  TransactionStatus_ACTIVE,
  TransactionStatus_COMMITTED,
  TransactionStatus_COMMIT_IN_PROGRESS,
  TransactionStatus'
  #-}
