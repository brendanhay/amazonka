{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
  ( OriginRequestPolicyCookieBehavior
      ( OriginRequestPolicyCookieBehavior',
        ORPCBAll,
        ORPCBNone,
        ORPCBWhitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OriginRequestPolicyCookieBehavior = OriginRequestPolicyCookieBehavior' Lude.Text
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

pattern ORPCBAll :: OriginRequestPolicyCookieBehavior
pattern ORPCBAll = OriginRequestPolicyCookieBehavior' "all"

pattern ORPCBNone :: OriginRequestPolicyCookieBehavior
pattern ORPCBNone = OriginRequestPolicyCookieBehavior' "none"

pattern ORPCBWhitelist :: OriginRequestPolicyCookieBehavior
pattern ORPCBWhitelist = OriginRequestPolicyCookieBehavior' "whitelist"

{-# COMPLETE
  ORPCBAll,
  ORPCBNone,
  ORPCBWhitelist,
  OriginRequestPolicyCookieBehavior'
  #-}
