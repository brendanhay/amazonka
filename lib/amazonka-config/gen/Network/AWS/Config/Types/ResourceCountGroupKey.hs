-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCountGroupKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountGroupKey
  ( ResourceCountGroupKey
      ( ResourceCountGroupKey',
        RCGKAWSRegion,
        RCGKAccountId,
        RCGKResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceCountGroupKey = ResourceCountGroupKey' Lude.Text
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

pattern RCGKAWSRegion :: ResourceCountGroupKey
pattern RCGKAWSRegion = ResourceCountGroupKey' "AWS_REGION"

pattern RCGKAccountId :: ResourceCountGroupKey
pattern RCGKAccountId = ResourceCountGroupKey' "ACCOUNT_ID"

pattern RCGKResourceType :: ResourceCountGroupKey
pattern RCGKResourceType = ResourceCountGroupKey' "RESOURCE_TYPE"

{-# COMPLETE
  RCGKAWSRegion,
  RCGKAccountId,
  RCGKResourceType,
  ResourceCountGroupKey'
  #-}
