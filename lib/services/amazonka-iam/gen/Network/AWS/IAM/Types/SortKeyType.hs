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
-- Module      : Network.AWS.IAM.Types.SortKeyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SortKeyType
  ( SortKeyType
      ( ..,
        SortKeyType_LAST_AUTHENTICATED_TIME_ASCENDING,
        SortKeyType_LAST_AUTHENTICATED_TIME_DESCENDING,
        SortKeyType_SERVICE_NAMESPACE_ASCENDING,
        SortKeyType_SERVICE_NAMESPACE_DESCENDING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SortKeyType = SortKeyType'
  { fromSortKeyType ::
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

pattern SortKeyType_LAST_AUTHENTICATED_TIME_ASCENDING :: SortKeyType
pattern SortKeyType_LAST_AUTHENTICATED_TIME_ASCENDING = SortKeyType' "LAST_AUTHENTICATED_TIME_ASCENDING"

pattern SortKeyType_LAST_AUTHENTICATED_TIME_DESCENDING :: SortKeyType
pattern SortKeyType_LAST_AUTHENTICATED_TIME_DESCENDING = SortKeyType' "LAST_AUTHENTICATED_TIME_DESCENDING"

pattern SortKeyType_SERVICE_NAMESPACE_ASCENDING :: SortKeyType
pattern SortKeyType_SERVICE_NAMESPACE_ASCENDING = SortKeyType' "SERVICE_NAMESPACE_ASCENDING"

pattern SortKeyType_SERVICE_NAMESPACE_DESCENDING :: SortKeyType
pattern SortKeyType_SERVICE_NAMESPACE_DESCENDING = SortKeyType' "SERVICE_NAMESPACE_DESCENDING"

{-# COMPLETE
  SortKeyType_LAST_AUTHENTICATED_TIME_ASCENDING,
  SortKeyType_LAST_AUTHENTICATED_TIME_DESCENDING,
  SortKeyType_SERVICE_NAMESPACE_ASCENDING,
  SortKeyType_SERVICE_NAMESPACE_DESCENDING,
  SortKeyType'
  #-}
