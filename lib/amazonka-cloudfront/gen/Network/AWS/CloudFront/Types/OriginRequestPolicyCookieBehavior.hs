{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
  ( OriginRequestPolicyCookieBehavior
    ( OriginRequestPolicyCookieBehavior'
    , OriginRequestPolicyCookieBehaviorNone
    , OriginRequestPolicyCookieBehaviorWhitelist
    , OriginRequestPolicyCookieBehaviorAll
    , fromOriginRequestPolicyCookieBehavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OriginRequestPolicyCookieBehavior = OriginRequestPolicyCookieBehavior'{fromOriginRequestPolicyCookieBehavior
                                                                               :: Core.Text}
                                              deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                              Core.Show, Core.Generic)
                                              deriving newtype (Core.IsString, Core.Hashable,
                                                                Core.NFData, Core.ToJSONKey,
                                                                Core.FromJSONKey, Core.ToJSON,
                                                                Core.FromJSON, Core.ToXML,
                                                                Core.FromXML, Core.ToText,
                                                                Core.FromText, Core.ToByteString,
                                                                Core.ToQuery, Core.ToHeader)

pattern OriginRequestPolicyCookieBehaviorNone :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehaviorNone = OriginRequestPolicyCookieBehavior' "none"

pattern OriginRequestPolicyCookieBehaviorWhitelist :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehaviorWhitelist = OriginRequestPolicyCookieBehavior' "whitelist"

pattern OriginRequestPolicyCookieBehaviorAll :: OriginRequestPolicyCookieBehavior
pattern OriginRequestPolicyCookieBehaviorAll = OriginRequestPolicyCookieBehavior' "all"

{-# COMPLETE 
  OriginRequestPolicyCookieBehaviorNone,

  OriginRequestPolicyCookieBehaviorWhitelist,

  OriginRequestPolicyCookieBehaviorAll,
  OriginRequestPolicyCookieBehavior'
  #-}
