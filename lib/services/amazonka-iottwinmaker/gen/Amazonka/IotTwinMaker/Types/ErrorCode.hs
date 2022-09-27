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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_INTERNAL_FAILURE,
        ErrorCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ErrorCode = ErrorCode'
  { fromErrorCode ::
      Core.Text
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

pattern ErrorCode_INTERNAL_FAILURE :: ErrorCode
pattern ErrorCode_INTERNAL_FAILURE = ErrorCode' "INTERNAL_FAILURE"

pattern ErrorCode_VALIDATION_ERROR :: ErrorCode
pattern ErrorCode_VALIDATION_ERROR = ErrorCode' "VALIDATION_ERROR"

{-# COMPLETE
  ErrorCode_INTERNAL_FAILURE,
  ErrorCode_VALIDATION_ERROR,
  ErrorCode'
  #-}
