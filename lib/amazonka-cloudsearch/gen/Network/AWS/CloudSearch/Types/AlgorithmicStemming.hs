{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AlgorithmicStemming
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AlgorithmicStemming where

import Network.AWS.Prelude

data AlgorithmicStemming
  = ASFull
  | ASLight
  | ASMinimal
  | ASNone
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

instance FromText AlgorithmicStemming where
  parser =
    takeLowerText >>= \case
      "full" -> pure ASFull
      "light" -> pure ASLight
      "minimal" -> pure ASMinimal
      "none" -> pure ASNone
      e ->
        fromTextError $
          "Failure parsing AlgorithmicStemming from value: '" <> e
            <> "'. Accepted values: full, light, minimal, none"

instance ToText AlgorithmicStemming where
  toText = \case
    ASFull -> "full"
    ASLight -> "light"
    ASMinimal -> "minimal"
    ASNone -> "none"

instance Hashable AlgorithmicStemming

instance NFData AlgorithmicStemming

instance ToByteString AlgorithmicStemming

instance ToQuery AlgorithmicStemming

instance ToHeader AlgorithmicStemming

instance FromXML AlgorithmicStemming where
  parseXML = parseXMLText "AlgorithmicStemming"
