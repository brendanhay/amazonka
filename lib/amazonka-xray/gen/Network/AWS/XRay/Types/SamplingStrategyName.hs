{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStrategyName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStrategyName where

import Network.AWS.Prelude

data SamplingStrategyName
  = FixedRate
  | PartialScan
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

instance FromText SamplingStrategyName where
  parser =
    takeLowerText >>= \case
      "fixedrate" -> pure FixedRate
      "partialscan" -> pure PartialScan
      e ->
        fromTextError $
          "Failure parsing SamplingStrategyName from value: '" <> e
            <> "'. Accepted values: fixedrate, partialscan"

instance ToText SamplingStrategyName where
  toText = \case
    FixedRate -> "FixedRate"
    PartialScan -> "PartialScan"

instance Hashable SamplingStrategyName

instance NFData SamplingStrategyName

instance ToByteString SamplingStrategyName

instance ToQuery SamplingStrategyName

instance ToHeader SamplingStrategyName

instance ToJSON SamplingStrategyName where
  toJSON = toJSONText
