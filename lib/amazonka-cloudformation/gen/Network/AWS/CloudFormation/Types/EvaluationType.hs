{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.EvaluationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.EvaluationType where

import Network.AWS.Prelude

data EvaluationType
  = ETDynamic
  | ETStatic
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

instance FromText EvaluationType where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure ETDynamic
      "static" -> pure ETStatic
      e ->
        fromTextError $
          "Failure parsing EvaluationType from value: '" <> e
            <> "'. Accepted values: dynamic, static"

instance ToText EvaluationType where
  toText = \case
    ETDynamic -> "Dynamic"
    ETStatic -> "Static"

instance Hashable EvaluationType

instance NFData EvaluationType

instance ToByteString EvaluationType

instance ToQuery EvaluationType

instance ToHeader EvaluationType

instance FromXML EvaluationType where
  parseXML = parseXMLText "EvaluationType"
