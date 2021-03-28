{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCountGroupKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ResourceCountGroupKey
  ( ResourceCountGroupKey
    ( ResourceCountGroupKey'
    , ResourceCountGroupKeyResourceType
    , ResourceCountGroupKeyAccountId
    , ResourceCountGroupKeyAwsRegion
    , fromResourceCountGroupKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ResourceCountGroupKey = ResourceCountGroupKey'{fromResourceCountGroupKey
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ResourceCountGroupKeyResourceType :: ResourceCountGroupKey
pattern ResourceCountGroupKeyResourceType = ResourceCountGroupKey' "RESOURCE_TYPE"

pattern ResourceCountGroupKeyAccountId :: ResourceCountGroupKey
pattern ResourceCountGroupKeyAccountId = ResourceCountGroupKey' "ACCOUNT_ID"

pattern ResourceCountGroupKeyAwsRegion :: ResourceCountGroupKey
pattern ResourceCountGroupKeyAwsRegion = ResourceCountGroupKey' "AWS_REGION"

{-# COMPLETE 
  ResourceCountGroupKeyResourceType,

  ResourceCountGroupKeyAccountId,

  ResourceCountGroupKeyAwsRegion,
  ResourceCountGroupKey'
  #-}
