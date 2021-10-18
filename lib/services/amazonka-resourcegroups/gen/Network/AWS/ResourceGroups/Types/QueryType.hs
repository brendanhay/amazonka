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
-- Module      : Network.AWS.ResourceGroups.Types.QueryType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryType
  ( QueryType
      ( ..,
        QueryType_CLOUDFORMATION_STACK_1_0,
        QueryType_TAG_FILTERS_1_0
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype QueryType = QueryType'
  { fromQueryType ::
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

pattern QueryType_CLOUDFORMATION_STACK_1_0 :: QueryType
pattern QueryType_CLOUDFORMATION_STACK_1_0 = QueryType' "CLOUDFORMATION_STACK_1_0"

pattern QueryType_TAG_FILTERS_1_0 :: QueryType
pattern QueryType_TAG_FILTERS_1_0 = QueryType' "TAG_FILTERS_1_0"

{-# COMPLETE
  QueryType_CLOUDFORMATION_STACK_1_0,
  QueryType_TAG_FILTERS_1_0,
  QueryType'
  #-}
