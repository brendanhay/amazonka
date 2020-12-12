{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
  ( GroupByAttribute
      ( GroupByAttribute',
        Region,
        ResourceType,
        TargetId
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GroupByAttribute = GroupByAttribute' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Region :: GroupByAttribute
pattern Region = GroupByAttribute' "REGION"

pattern ResourceType :: GroupByAttribute
pattern ResourceType = GroupByAttribute' "RESOURCE_TYPE"

pattern TargetId :: GroupByAttribute
pattern TargetId = GroupByAttribute' "TARGET_ID"

{-# COMPLETE
  Region,
  ResourceType,
  TargetId,
  GroupByAttribute'
  #-}
