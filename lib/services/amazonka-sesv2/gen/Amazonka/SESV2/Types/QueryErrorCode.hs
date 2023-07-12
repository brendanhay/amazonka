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
-- Module      : Amazonka.SESV2.Types.QueryErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.QueryErrorCode
  ( QueryErrorCode
      ( ..,
        QueryErrorCode_ACCESS_DENIED,
        QueryErrorCode_INTERNAL_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueryErrorCode = QueryErrorCode'
  { fromQueryErrorCode ::
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

pattern QueryErrorCode_ACCESS_DENIED :: QueryErrorCode
pattern QueryErrorCode_ACCESS_DENIED = QueryErrorCode' "ACCESS_DENIED"

pattern QueryErrorCode_INTERNAL_FAILURE :: QueryErrorCode
pattern QueryErrorCode_INTERNAL_FAILURE = QueryErrorCode' "INTERNAL_FAILURE"

{-# COMPLETE
  QueryErrorCode_ACCESS_DENIED,
  QueryErrorCode_INTERNAL_FAILURE,
  QueryErrorCode'
  #-}
