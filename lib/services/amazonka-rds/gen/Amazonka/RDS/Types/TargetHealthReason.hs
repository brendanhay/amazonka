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
-- Module      : Amazonka.RDS.Types.TargetHealthReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.TargetHealthReason
  ( TargetHealthReason
      ( ..,
        TargetHealthReason_AUTH_FAILURE,
        TargetHealthReason_CONNECTION_FAILED,
        TargetHealthReason_INVALID_REPLICATION_STATE,
        TargetHealthReason_PENDING_PROXY_CAPACITY,
        TargetHealthReason_UNREACHABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetHealthReason = TargetHealthReason'
  { fromTargetHealthReason ::
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

pattern TargetHealthReason_AUTH_FAILURE :: TargetHealthReason
pattern TargetHealthReason_AUTH_FAILURE = TargetHealthReason' "AUTH_FAILURE"

pattern TargetHealthReason_CONNECTION_FAILED :: TargetHealthReason
pattern TargetHealthReason_CONNECTION_FAILED = TargetHealthReason' "CONNECTION_FAILED"

pattern TargetHealthReason_INVALID_REPLICATION_STATE :: TargetHealthReason
pattern TargetHealthReason_INVALID_REPLICATION_STATE = TargetHealthReason' "INVALID_REPLICATION_STATE"

pattern TargetHealthReason_PENDING_PROXY_CAPACITY :: TargetHealthReason
pattern TargetHealthReason_PENDING_PROXY_CAPACITY = TargetHealthReason' "PENDING_PROXY_CAPACITY"

pattern TargetHealthReason_UNREACHABLE :: TargetHealthReason
pattern TargetHealthReason_UNREACHABLE = TargetHealthReason' "UNREACHABLE"

{-# COMPLETE
  TargetHealthReason_AUTH_FAILURE,
  TargetHealthReason_CONNECTION_FAILED,
  TargetHealthReason_INVALID_REPLICATION_STATE,
  TargetHealthReason_PENDING_PROXY_CAPACITY,
  TargetHealthReason_UNREACHABLE,
  TargetHealthReason'
  #-}
