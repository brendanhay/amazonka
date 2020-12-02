{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitFeatureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimitFeatureType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitFeatureType
  = ConcurrencyScaling
  | Spectrum
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText UsageLimitFeatureType where
  parser =
    takeLowerText >>= \case
      "concurrency-scaling" -> pure ConcurrencyScaling
      "spectrum" -> pure Spectrum
      e ->
        fromTextError $
          "Failure parsing UsageLimitFeatureType from value: '" <> e
            <> "'. Accepted values: concurrency-scaling, spectrum"

instance ToText UsageLimitFeatureType where
  toText = \case
    ConcurrencyScaling -> "concurrency-scaling"
    Spectrum -> "spectrum"

instance Hashable UsageLimitFeatureType

instance NFData UsageLimitFeatureType

instance ToByteString UsageLimitFeatureType

instance ToQuery UsageLimitFeatureType

instance ToHeader UsageLimitFeatureType

instance FromXML UsageLimitFeatureType where
  parseXML = parseXMLText "UsageLimitFeatureType"
