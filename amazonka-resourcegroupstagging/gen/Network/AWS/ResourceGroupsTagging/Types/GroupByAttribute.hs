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

import qualified Network.AWS.Prelude as Prelude

newtype GroupByAttribute = GroupByAttribute'
  { fromGroupByAttribute ::
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
