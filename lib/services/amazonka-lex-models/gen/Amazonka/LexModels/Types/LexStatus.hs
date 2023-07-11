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
-- Module      : Amazonka.LexModels.Types.LexStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.LexStatus
  ( LexStatus
      ( ..,
        LexStatus_BUILDING,
        LexStatus_FAILED,
        LexStatus_NOT_BUILT,
        LexStatus_READY,
        LexStatus_READY_BASIC_TESTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LexStatus = LexStatus'
  { fromLexStatus ::
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

pattern LexStatus_BUILDING :: LexStatus
pattern LexStatus_BUILDING = LexStatus' "BUILDING"

pattern LexStatus_FAILED :: LexStatus
pattern LexStatus_FAILED = LexStatus' "FAILED"

pattern LexStatus_NOT_BUILT :: LexStatus
pattern LexStatus_NOT_BUILT = LexStatus' "NOT_BUILT"

pattern LexStatus_READY :: LexStatus
pattern LexStatus_READY = LexStatus' "READY"

pattern LexStatus_READY_BASIC_TESTING :: LexStatus
pattern LexStatus_READY_BASIC_TESTING = LexStatus' "READY_BASIC_TESTING"

{-# COMPLETE
  LexStatus_BUILDING,
  LexStatus_FAILED,
  LexStatus_NOT_BUILT,
  LexStatus_READY,
  LexStatus_READY_BASIC_TESTING,
  LexStatus'
  #-}
