-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestrictionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.GeoRestrictionType
  ( GeoRestrictionType
      ( GeoRestrictionType',
        GRTBlacklist,
        GRTNone,
        GRTWhitelist
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GeoRestrictionType = GeoRestrictionType' Lude.Text
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

pattern GRTBlacklist :: GeoRestrictionType
pattern GRTBlacklist = GeoRestrictionType' "blacklist"

pattern GRTNone :: GeoRestrictionType
pattern GRTNone = GeoRestrictionType' "none"

pattern GRTWhitelist :: GeoRestrictionType
pattern GRTWhitelist = GeoRestrictionType' "whitelist"

{-# COMPLETE
  GRTBlacklist,
  GRTNone,
  GRTWhitelist,
  GeoRestrictionType'
  #-}
