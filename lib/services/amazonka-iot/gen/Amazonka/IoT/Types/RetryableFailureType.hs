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
-- Module      : Amazonka.IoT.Types.RetryableFailureType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RetryableFailureType
  ( RetryableFailureType
      ( ..,
        RetryableFailureType_ALL,
        RetryableFailureType_FAILED,
        RetryableFailureType_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RetryableFailureType = RetryableFailureType'
  { fromRetryableFailureType ::
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

pattern RetryableFailureType_ALL :: RetryableFailureType
pattern RetryableFailureType_ALL = RetryableFailureType' "ALL"

pattern RetryableFailureType_FAILED :: RetryableFailureType
pattern RetryableFailureType_FAILED = RetryableFailureType' "FAILED"

pattern RetryableFailureType_TIMED_OUT :: RetryableFailureType
pattern RetryableFailureType_TIMED_OUT = RetryableFailureType' "TIMED_OUT"

{-# COMPLETE
  RetryableFailureType_ALL,
  RetryableFailureType_FAILED,
  RetryableFailureType_TIMED_OUT,
  RetryableFailureType'
  #-}
