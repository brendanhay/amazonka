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
-- Module      : Amazonka.SecurityHub.Types.UnprocessedErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.UnprocessedErrorCode
  ( UnprocessedErrorCode
      ( ..,
        UnprocessedErrorCode_ACCESS_DENIED,
        UnprocessedErrorCode_INVALID_INPUT,
        UnprocessedErrorCode_LIMIT_EXCEEDED,
        UnprocessedErrorCode_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnprocessedErrorCode = UnprocessedErrorCode'
  { fromUnprocessedErrorCode ::
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

pattern UnprocessedErrorCode_ACCESS_DENIED :: UnprocessedErrorCode
pattern UnprocessedErrorCode_ACCESS_DENIED = UnprocessedErrorCode' "ACCESS_DENIED"

pattern UnprocessedErrorCode_INVALID_INPUT :: UnprocessedErrorCode
pattern UnprocessedErrorCode_INVALID_INPUT = UnprocessedErrorCode' "INVALID_INPUT"

pattern UnprocessedErrorCode_LIMIT_EXCEEDED :: UnprocessedErrorCode
pattern UnprocessedErrorCode_LIMIT_EXCEEDED = UnprocessedErrorCode' "LIMIT_EXCEEDED"

pattern UnprocessedErrorCode_NOT_FOUND :: UnprocessedErrorCode
pattern UnprocessedErrorCode_NOT_FOUND = UnprocessedErrorCode' "NOT_FOUND"

{-# COMPLETE
  UnprocessedErrorCode_ACCESS_DENIED,
  UnprocessedErrorCode_INVALID_INPUT,
  UnprocessedErrorCode_LIMIT_EXCEEDED,
  UnprocessedErrorCode_NOT_FOUND,
  UnprocessedErrorCode'
  #-}
