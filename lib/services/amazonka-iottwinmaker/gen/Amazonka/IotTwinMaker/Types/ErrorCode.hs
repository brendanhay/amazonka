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
-- Module      : Amazonka.IotTwinMaker.Types.ErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_INTERNAL_FAILURE,
        ErrorCode_SYNC_CREATING_ERROR,
        ErrorCode_SYNC_INITIALIZING_ERROR,
        ErrorCode_SYNC_PROCESSING_ERROR,
        ErrorCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorCode = ErrorCode'
  { fromErrorCode ::
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

pattern ErrorCode_INTERNAL_FAILURE :: ErrorCode
pattern ErrorCode_INTERNAL_FAILURE = ErrorCode' "INTERNAL_FAILURE"

pattern ErrorCode_SYNC_CREATING_ERROR :: ErrorCode
pattern ErrorCode_SYNC_CREATING_ERROR = ErrorCode' "SYNC_CREATING_ERROR"

pattern ErrorCode_SYNC_INITIALIZING_ERROR :: ErrorCode
pattern ErrorCode_SYNC_INITIALIZING_ERROR = ErrorCode' "SYNC_INITIALIZING_ERROR"

pattern ErrorCode_SYNC_PROCESSING_ERROR :: ErrorCode
pattern ErrorCode_SYNC_PROCESSING_ERROR = ErrorCode' "SYNC_PROCESSING_ERROR"

pattern ErrorCode_VALIDATION_ERROR :: ErrorCode
pattern ErrorCode_VALIDATION_ERROR = ErrorCode' "VALIDATION_ERROR"

{-# COMPLETE
  ErrorCode_INTERNAL_FAILURE,
  ErrorCode_SYNC_CREATING_ERROR,
  ErrorCode_SYNC_INITIALIZING_ERROR,
  ErrorCode_SYNC_PROCESSING_ERROR,
  ErrorCode_VALIDATION_ERROR,
  ErrorCode'
  #-}
