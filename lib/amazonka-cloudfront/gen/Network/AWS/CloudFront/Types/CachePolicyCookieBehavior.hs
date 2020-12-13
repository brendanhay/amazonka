{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
  ( CachePolicyCookieBehavior
      ( CachePolicyCookieBehavior',
        CPCBNone,
        CPCBWhitelist,
        CPCBAllExcept,
        CPCBAll
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CachePolicyCookieBehavior = CachePolicyCookieBehavior' Lude.Text
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

pattern CPCBNone :: CachePolicyCookieBehavior
pattern CPCBNone = CachePolicyCookieBehavior' "none"

pattern CPCBWhitelist :: CachePolicyCookieBehavior
pattern CPCBWhitelist = CachePolicyCookieBehavior' "whitelist"

pattern CPCBAllExcept :: CachePolicyCookieBehavior
pattern CPCBAllExcept = CachePolicyCookieBehavior' "allExcept"

pattern CPCBAll :: CachePolicyCookieBehavior
pattern CPCBAll = CachePolicyCookieBehavior' "all"

{-# COMPLETE
  CPCBNone,
  CPCBWhitelist,
  CPCBAllExcept,
  CPCBAll,
  CachePolicyCookieBehavior'
  #-}
