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
-- Module      : Amazonka.Kendra.Types.QueryResultType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.QueryResultType
  ( QueryResultType
      ( ..,
        QueryResultType_ANSWER,
        QueryResultType_DOCUMENT,
        QueryResultType_QUESTION_ANSWER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueryResultType = QueryResultType'
  { fromQueryResultType ::
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

pattern QueryResultType_ANSWER :: QueryResultType
pattern QueryResultType_ANSWER = QueryResultType' "ANSWER"

pattern QueryResultType_DOCUMENT :: QueryResultType
pattern QueryResultType_DOCUMENT = QueryResultType' "DOCUMENT"

pattern QueryResultType_QUESTION_ANSWER :: QueryResultType
pattern QueryResultType_QUESTION_ANSWER = QueryResultType' "QUESTION_ANSWER"

{-# COMPLETE
  QueryResultType_ANSWER,
  QueryResultType_DOCUMENT,
  QueryResultType_QUESTION_ANSWER,
  QueryResultType'
  #-}
