-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ReusableDelegationSetLimitType
  ( ReusableDelegationSetLimitType
      ( ReusableDelegationSetLimitType',
        MaxZonesByReusableDelegationSet
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype ReusableDelegationSetLimitType = ReusableDelegationSetLimitType' Lude.Text
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

pattern MaxZonesByReusableDelegationSet :: ReusableDelegationSetLimitType
pattern MaxZonesByReusableDelegationSet = ReusableDelegationSetLimitType' "MAX_ZONES_BY_REUSABLE_DELEGATION_SET"

{-# COMPLETE
  MaxZonesByReusableDelegationSet,
  ReusableDelegationSetLimitType'
  #-}
