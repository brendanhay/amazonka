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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
  ( GroupByAttribute
      ( ..,
        GroupByAttribute_REGION,
        GroupByAttribute_RESOURCE_TYPE,
        GroupByAttribute_TARGET_ID
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GroupByAttribute = GroupByAttribute'
  { fromGroupByAttribute ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern GroupByAttribute_REGION :: GroupByAttribute
pattern GroupByAttribute_REGION = GroupByAttribute' "REGION"

pattern GroupByAttribute_RESOURCE_TYPE :: GroupByAttribute
pattern GroupByAttribute_RESOURCE_TYPE = GroupByAttribute' "RESOURCE_TYPE"

pattern GroupByAttribute_TARGET_ID :: GroupByAttribute
pattern GroupByAttribute_TARGET_ID = GroupByAttribute' "TARGET_ID"

{-# COMPLETE
  GroupByAttribute_REGION,
  GroupByAttribute_RESOURCE_TYPE,
  GroupByAttribute_TARGET_ID,
  GroupByAttribute'
  #-}
