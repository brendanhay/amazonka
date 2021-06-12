{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestrictionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.GeoRestrictionType
  ( GeoRestrictionType
      ( ..,
        GeoRestrictionType_Blacklist,
        GeoRestrictionType_None,
        GeoRestrictionType_Whitelist
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GeoRestrictionType = GeoRestrictionType'
  { fromGeoRestrictionType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern GeoRestrictionType_Blacklist :: GeoRestrictionType
pattern GeoRestrictionType_Blacklist = GeoRestrictionType' "blacklist"

pattern GeoRestrictionType_None :: GeoRestrictionType
pattern GeoRestrictionType_None = GeoRestrictionType' "none"

pattern GeoRestrictionType_Whitelist :: GeoRestrictionType
pattern GeoRestrictionType_Whitelist = GeoRestrictionType' "whitelist"

{-# COMPLETE
  GeoRestrictionType_Blacklist,
  GeoRestrictionType_None,
  GeoRestrictionType_Whitelist,
  GeoRestrictionType'
  #-}
