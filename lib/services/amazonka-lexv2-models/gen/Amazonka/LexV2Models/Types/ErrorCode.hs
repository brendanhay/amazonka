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
-- Module      : Amazonka.LexV2Models.Types.ErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_DUPLICATE_INPUT,
        ErrorCode_INTERNAL_SERVER_FAILURE,
        ErrorCode_RESOURCE_ALREADY_EXISTS,
        ErrorCode_RESOURCE_DOES_NOT_EXIST
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

pattern ErrorCode_DUPLICATE_INPUT :: ErrorCode
pattern ErrorCode_DUPLICATE_INPUT = ErrorCode' "DUPLICATE_INPUT"

pattern ErrorCode_INTERNAL_SERVER_FAILURE :: ErrorCode
pattern ErrorCode_INTERNAL_SERVER_FAILURE = ErrorCode' "INTERNAL_SERVER_FAILURE"

pattern ErrorCode_RESOURCE_ALREADY_EXISTS :: ErrorCode
pattern ErrorCode_RESOURCE_ALREADY_EXISTS = ErrorCode' "RESOURCE_ALREADY_EXISTS"

pattern ErrorCode_RESOURCE_DOES_NOT_EXIST :: ErrorCode
pattern ErrorCode_RESOURCE_DOES_NOT_EXIST = ErrorCode' "RESOURCE_DOES_NOT_EXIST"

{-# COMPLETE
  ErrorCode_DUPLICATE_INPUT,
  ErrorCode_INTERNAL_SERVER_FAILURE,
  ErrorCode_RESOURCE_ALREADY_EXISTS,
  ErrorCode_RESOURCE_DOES_NOT_EXIST,
  ErrorCode'
  #-}
