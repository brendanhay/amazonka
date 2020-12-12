{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
  ( CachePolicyQueryStringBehavior
      ( CachePolicyQueryStringBehavior',
        CPQSBAll,
        CPQSBAllExcept,
        CPQSBNone,
        CPQSBWhitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CachePolicyQueryStringBehavior = CachePolicyQueryStringBehavior' Lude.Text
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

pattern CPQSBAll :: CachePolicyQueryStringBehavior
pattern CPQSBAll = CachePolicyQueryStringBehavior' "all"

pattern CPQSBAllExcept :: CachePolicyQueryStringBehavior
pattern CPQSBAllExcept = CachePolicyQueryStringBehavior' "allExcept"

pattern CPQSBNone :: CachePolicyQueryStringBehavior
pattern CPQSBNone = CachePolicyQueryStringBehavior' "none"

pattern CPQSBWhitelist :: CachePolicyQueryStringBehavior
pattern CPQSBWhitelist = CachePolicyQueryStringBehavior' "whitelist"

{-# COMPLETE
  CPQSBAll,
  CPQSBAllExcept,
  CPQSBNone,
  CPQSBWhitelist,
  CachePolicyQueryStringBehavior'
  #-}
