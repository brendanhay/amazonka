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
-- Module      : Amazonka.Synthetics.Types.CanaryStateReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryStateReasonCode
  ( CanaryStateReasonCode
      ( ..,
        CanaryStateReasonCode_CREATE_FAILED,
        CanaryStateReasonCode_CREATE_IN_PROGRESS,
        CanaryStateReasonCode_CREATE_PENDING,
        CanaryStateReasonCode_DELETE_FAILED,
        CanaryStateReasonCode_DELETE_IN_PROGRESS,
        CanaryStateReasonCode_INVALID_PERMISSIONS,
        CanaryStateReasonCode_ROLLBACK_COMPLETE,
        CanaryStateReasonCode_ROLLBACK_FAILED,
        CanaryStateReasonCode_SYNC_DELETE_IN_PROGRESS,
        CanaryStateReasonCode_UPDATE_COMPLETE,
        CanaryStateReasonCode_UPDATE_IN_PROGRESS,
        CanaryStateReasonCode_UPDATE_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CanaryStateReasonCode = CanaryStateReasonCode'
  { fromCanaryStateReasonCode ::
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

pattern CanaryStateReasonCode_CREATE_FAILED :: CanaryStateReasonCode
pattern CanaryStateReasonCode_CREATE_FAILED = CanaryStateReasonCode' "CREATE_FAILED"

pattern CanaryStateReasonCode_CREATE_IN_PROGRESS :: CanaryStateReasonCode
pattern CanaryStateReasonCode_CREATE_IN_PROGRESS = CanaryStateReasonCode' "CREATE_IN_PROGRESS"

pattern CanaryStateReasonCode_CREATE_PENDING :: CanaryStateReasonCode
pattern CanaryStateReasonCode_CREATE_PENDING = CanaryStateReasonCode' "CREATE_PENDING"

pattern CanaryStateReasonCode_DELETE_FAILED :: CanaryStateReasonCode
pattern CanaryStateReasonCode_DELETE_FAILED = CanaryStateReasonCode' "DELETE_FAILED"

pattern CanaryStateReasonCode_DELETE_IN_PROGRESS :: CanaryStateReasonCode
pattern CanaryStateReasonCode_DELETE_IN_PROGRESS = CanaryStateReasonCode' "DELETE_IN_PROGRESS"

pattern CanaryStateReasonCode_INVALID_PERMISSIONS :: CanaryStateReasonCode
pattern CanaryStateReasonCode_INVALID_PERMISSIONS = CanaryStateReasonCode' "INVALID_PERMISSIONS"

pattern CanaryStateReasonCode_ROLLBACK_COMPLETE :: CanaryStateReasonCode
pattern CanaryStateReasonCode_ROLLBACK_COMPLETE = CanaryStateReasonCode' "ROLLBACK_COMPLETE"

pattern CanaryStateReasonCode_ROLLBACK_FAILED :: CanaryStateReasonCode
pattern CanaryStateReasonCode_ROLLBACK_FAILED = CanaryStateReasonCode' "ROLLBACK_FAILED"

pattern CanaryStateReasonCode_SYNC_DELETE_IN_PROGRESS :: CanaryStateReasonCode
pattern CanaryStateReasonCode_SYNC_DELETE_IN_PROGRESS = CanaryStateReasonCode' "SYNC_DELETE_IN_PROGRESS"

pattern CanaryStateReasonCode_UPDATE_COMPLETE :: CanaryStateReasonCode
pattern CanaryStateReasonCode_UPDATE_COMPLETE = CanaryStateReasonCode' "UPDATE_COMPLETE"

pattern CanaryStateReasonCode_UPDATE_IN_PROGRESS :: CanaryStateReasonCode
pattern CanaryStateReasonCode_UPDATE_IN_PROGRESS = CanaryStateReasonCode' "UPDATE_IN_PROGRESS"

pattern CanaryStateReasonCode_UPDATE_PENDING :: CanaryStateReasonCode
pattern CanaryStateReasonCode_UPDATE_PENDING = CanaryStateReasonCode' "UPDATE_PENDING"

{-# COMPLETE
  CanaryStateReasonCode_CREATE_FAILED,
  CanaryStateReasonCode_CREATE_IN_PROGRESS,
  CanaryStateReasonCode_CREATE_PENDING,
  CanaryStateReasonCode_DELETE_FAILED,
  CanaryStateReasonCode_DELETE_IN_PROGRESS,
  CanaryStateReasonCode_INVALID_PERMISSIONS,
  CanaryStateReasonCode_ROLLBACK_COMPLETE,
  CanaryStateReasonCode_ROLLBACK_FAILED,
  CanaryStateReasonCode_SYNC_DELETE_IN_PROGRESS,
  CanaryStateReasonCode_UPDATE_COMPLETE,
  CanaryStateReasonCode_UPDATE_IN_PROGRESS,
  CanaryStateReasonCode_UPDATE_PENDING,
  CanaryStateReasonCode'
  #-}
