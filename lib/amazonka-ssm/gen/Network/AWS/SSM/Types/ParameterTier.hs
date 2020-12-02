{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterTier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterTier where

import Network.AWS.Prelude

data ParameterTier
  = Advanced
  | IntelligentTiering
  | Standard
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

instance FromText ParameterTier where
  parser =
    takeLowerText >>= \case
      "advanced" -> pure Advanced
      "intelligent-tiering" -> pure IntelligentTiering
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing ParameterTier from value: '" <> e
            <> "'. Accepted values: advanced, intelligent-tiering, standard"

instance ToText ParameterTier where
  toText = \case
    Advanced -> "Advanced"
    IntelligentTiering -> "Intelligent-Tiering"
    Standard -> "Standard"

instance Hashable ParameterTier

instance NFData ParameterTier

instance ToByteString ParameterTier

instance ToQuery ParameterTier

instance ToHeader ParameterTier

instance ToJSON ParameterTier where
  toJSON = toJSONText

instance FromJSON ParameterTier where
  parseJSON = parseJSONText "ParameterTier"
