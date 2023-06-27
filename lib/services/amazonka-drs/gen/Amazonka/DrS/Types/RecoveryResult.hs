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
-- Module      : Amazonka.DrS.Types.RecoveryResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryResult
  ( RecoveryResult
      ( ..,
        RecoveryResult_ASSOCIATE_FAIL,
        RecoveryResult_ASSOCIATE_SUCCESS,
        RecoveryResult_FAIL,
        RecoveryResult_IN_PROGRESS,
        RecoveryResult_NOT_STARTED,
        RecoveryResult_PARTIAL_SUCCESS,
        RecoveryResult_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecoveryResult = RecoveryResult'
  { fromRecoveryResult ::
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

pattern RecoveryResult_ASSOCIATE_FAIL :: RecoveryResult
pattern RecoveryResult_ASSOCIATE_FAIL = RecoveryResult' "ASSOCIATE_FAIL"

pattern RecoveryResult_ASSOCIATE_SUCCESS :: RecoveryResult
pattern RecoveryResult_ASSOCIATE_SUCCESS = RecoveryResult' "ASSOCIATE_SUCCESS"

pattern RecoveryResult_FAIL :: RecoveryResult
pattern RecoveryResult_FAIL = RecoveryResult' "FAIL"

pattern RecoveryResult_IN_PROGRESS :: RecoveryResult
pattern RecoveryResult_IN_PROGRESS = RecoveryResult' "IN_PROGRESS"

pattern RecoveryResult_NOT_STARTED :: RecoveryResult
pattern RecoveryResult_NOT_STARTED = RecoveryResult' "NOT_STARTED"

pattern RecoveryResult_PARTIAL_SUCCESS :: RecoveryResult
pattern RecoveryResult_PARTIAL_SUCCESS = RecoveryResult' "PARTIAL_SUCCESS"

pattern RecoveryResult_SUCCESS :: RecoveryResult
pattern RecoveryResult_SUCCESS = RecoveryResult' "SUCCESS"

{-# COMPLETE
  RecoveryResult_ASSOCIATE_FAIL,
  RecoveryResult_ASSOCIATE_SUCCESS,
  RecoveryResult_FAIL,
  RecoveryResult_IN_PROGRESS,
  RecoveryResult_NOT_STARTED,
  RecoveryResult_PARTIAL_SUCCESS,
  RecoveryResult_SUCCESS,
  RecoveryResult'
  #-}
