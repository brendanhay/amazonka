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
-- Module      : Network.AWS.Kendra.Types.QuerySuggestionsBlockListStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.QuerySuggestionsBlockListStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype QuerySuggestionsBlockListStatus = QuerySuggestionsBlockListStatus'
  { fromQuerySuggestionsBlockListStatus ::
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
