{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype QueryType = QueryType'
  { fromQueryType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
