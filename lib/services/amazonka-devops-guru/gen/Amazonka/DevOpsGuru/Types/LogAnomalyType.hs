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
-- Module      : Amazonka.DevOpsGuru.Types.LogAnomalyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.LogAnomalyType
  ( LogAnomalyType
      ( ..,
        LogAnomalyType_BLOCK_FORMAT,
        LogAnomalyType_FORMAT,
        LogAnomalyType_HTTP_CODE,
        LogAnomalyType_KEYWORD,
        LogAnomalyType_KEYWORD_TOKEN,
        LogAnomalyType_NEW_FIELD_NAME,
        LogAnomalyType_NUMERICAL_NAN,
        LogAnomalyType_NUMERICAL_POINT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LogAnomalyType = LogAnomalyType'
  { fromLogAnomalyType ::
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

pattern LogAnomalyType_BLOCK_FORMAT :: LogAnomalyType
pattern LogAnomalyType_BLOCK_FORMAT = LogAnomalyType' "BLOCK_FORMAT"

pattern LogAnomalyType_FORMAT :: LogAnomalyType
pattern LogAnomalyType_FORMAT = LogAnomalyType' "FORMAT"

pattern LogAnomalyType_HTTP_CODE :: LogAnomalyType
pattern LogAnomalyType_HTTP_CODE = LogAnomalyType' "HTTP_CODE"

pattern LogAnomalyType_KEYWORD :: LogAnomalyType
pattern LogAnomalyType_KEYWORD = LogAnomalyType' "KEYWORD"

pattern LogAnomalyType_KEYWORD_TOKEN :: LogAnomalyType
pattern LogAnomalyType_KEYWORD_TOKEN = LogAnomalyType' "KEYWORD_TOKEN"

pattern LogAnomalyType_NEW_FIELD_NAME :: LogAnomalyType
pattern LogAnomalyType_NEW_FIELD_NAME = LogAnomalyType' "NEW_FIELD_NAME"

pattern LogAnomalyType_NUMERICAL_NAN :: LogAnomalyType
pattern LogAnomalyType_NUMERICAL_NAN = LogAnomalyType' "NUMERICAL_NAN"

pattern LogAnomalyType_NUMERICAL_POINT :: LogAnomalyType
pattern LogAnomalyType_NUMERICAL_POINT = LogAnomalyType' "NUMERICAL_POINT"

{-# COMPLETE
  LogAnomalyType_BLOCK_FORMAT,
  LogAnomalyType_FORMAT,
  LogAnomalyType_HTTP_CODE,
  LogAnomalyType_KEYWORD,
  LogAnomalyType_KEYWORD_TOKEN,
  LogAnomalyType_NEW_FIELD_NAME,
  LogAnomalyType_NUMERICAL_NAN,
  LogAnomalyType_NUMERICAL_POINT,
  LogAnomalyType'
  #-}
