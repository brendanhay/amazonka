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
-- Module      : Network.AWS.LexModels.Types.LexStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LexStatus
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

import qualified Network.AWS.Core as Core

newtype LexStatus = LexStatus'
  { fromLexStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
