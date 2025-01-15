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
-- Module      : Amazonka.DynamoDB.Types.PointInTimeRecoveryStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.PointInTimeRecoveryStatus
  ( PointInTimeRecoveryStatus
      ( ..,
        PointInTimeRecoveryStatus_DISABLED,
        PointInTimeRecoveryStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype PointInTimeRecoveryStatus = PointInTimeRecoveryStatus'
  { fromPointInTimeRecoveryStatus ::
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

pattern PointInTimeRecoveryStatus_DISABLED :: PointInTimeRecoveryStatus
pattern PointInTimeRecoveryStatus_DISABLED = PointInTimeRecoveryStatus' "DISABLED"

pattern PointInTimeRecoveryStatus_ENABLED :: PointInTimeRecoveryStatus
pattern PointInTimeRecoveryStatus_ENABLED = PointInTimeRecoveryStatus' "ENABLED"

{-# COMPLETE
  PointInTimeRecoveryStatus_DISABLED,
  PointInTimeRecoveryStatus_ENABLED,
  PointInTimeRecoveryStatus'
  #-}
