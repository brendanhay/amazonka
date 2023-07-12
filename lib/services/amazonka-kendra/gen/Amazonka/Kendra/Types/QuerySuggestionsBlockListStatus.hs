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
-- Module      : Amazonka.Kendra.Types.QuerySuggestionsBlockListStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.QuerySuggestionsBlockListStatus
  ( QuerySuggestionsBlockListStatus
      ( ..,
        QuerySuggestionsBlockListStatus_ACTIVE,
        QuerySuggestionsBlockListStatus_ACTIVE_BUT_UPDATE_FAILED,
        QuerySuggestionsBlockListStatus_CREATING,
        QuerySuggestionsBlockListStatus_DELETING,
        QuerySuggestionsBlockListStatus_FAILED,
        QuerySuggestionsBlockListStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QuerySuggestionsBlockListStatus = QuerySuggestionsBlockListStatus'
  { fromQuerySuggestionsBlockListStatus ::
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

pattern QuerySuggestionsBlockListStatus_ACTIVE :: QuerySuggestionsBlockListStatus
pattern QuerySuggestionsBlockListStatus_ACTIVE = QuerySuggestionsBlockListStatus' "ACTIVE"

pattern QuerySuggestionsBlockListStatus_ACTIVE_BUT_UPDATE_FAILED :: QuerySuggestionsBlockListStatus
pattern QuerySuggestionsBlockListStatus_ACTIVE_BUT_UPDATE_FAILED = QuerySuggestionsBlockListStatus' "ACTIVE_BUT_UPDATE_FAILED"

pattern QuerySuggestionsBlockListStatus_CREATING :: QuerySuggestionsBlockListStatus
pattern QuerySuggestionsBlockListStatus_CREATING = QuerySuggestionsBlockListStatus' "CREATING"

pattern QuerySuggestionsBlockListStatus_DELETING :: QuerySuggestionsBlockListStatus
pattern QuerySuggestionsBlockListStatus_DELETING = QuerySuggestionsBlockListStatus' "DELETING"

pattern QuerySuggestionsBlockListStatus_FAILED :: QuerySuggestionsBlockListStatus
pattern QuerySuggestionsBlockListStatus_FAILED = QuerySuggestionsBlockListStatus' "FAILED"

pattern QuerySuggestionsBlockListStatus_UPDATING :: QuerySuggestionsBlockListStatus
pattern QuerySuggestionsBlockListStatus_UPDATING = QuerySuggestionsBlockListStatus' "UPDATING"

{-# COMPLETE
  QuerySuggestionsBlockListStatus_ACTIVE,
  QuerySuggestionsBlockListStatus_ACTIVE_BUT_UPDATE_FAILED,
  QuerySuggestionsBlockListStatus_CREATING,
  QuerySuggestionsBlockListStatus_DELETING,
  QuerySuggestionsBlockListStatus_FAILED,
  QuerySuggestionsBlockListStatus_UPDATING,
  QuerySuggestionsBlockListStatus'
  #-}
