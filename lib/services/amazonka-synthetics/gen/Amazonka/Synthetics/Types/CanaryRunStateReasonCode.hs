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
-- Module      : Amazonka.Synthetics.Types.CanaryRunStateReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryRunStateReasonCode
  ( CanaryRunStateReasonCode
      ( ..,
        CanaryRunStateReasonCode_CANARY_FAILURE,
        CanaryRunStateReasonCode_EXECUTION_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CanaryRunStateReasonCode = CanaryRunStateReasonCode'
  { fromCanaryRunStateReasonCode ::
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

pattern CanaryRunStateReasonCode_CANARY_FAILURE :: CanaryRunStateReasonCode
pattern CanaryRunStateReasonCode_CANARY_FAILURE = CanaryRunStateReasonCode' "CANARY_FAILURE"

pattern CanaryRunStateReasonCode_EXECUTION_FAILURE :: CanaryRunStateReasonCode
pattern CanaryRunStateReasonCode_EXECUTION_FAILURE = CanaryRunStateReasonCode' "EXECUTION_FAILURE"

{-# COMPLETE
  CanaryRunStateReasonCode_CANARY_FAILURE,
  CanaryRunStateReasonCode_EXECUTION_FAILURE,
  CanaryRunStateReasonCode'
  #-}
