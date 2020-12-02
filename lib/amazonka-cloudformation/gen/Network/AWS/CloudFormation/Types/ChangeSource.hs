{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSource where

import Network.AWS.Prelude

data ChangeSource
  = Automatic
  | DirectModification
  | ParameterReference
  | ResourceAttribute
  | ResourceReference
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

instance FromText ChangeSource where
  parser =
    takeLowerText >>= \case
      "automatic" -> pure Automatic
      "directmodification" -> pure DirectModification
      "parameterreference" -> pure ParameterReference
      "resourceattribute" -> pure ResourceAttribute
      "resourcereference" -> pure ResourceReference
      e ->
        fromTextError $
          "Failure parsing ChangeSource from value: '" <> e
            <> "'. Accepted values: automatic, directmodification, parameterreference, resourceattribute, resourcereference"

instance ToText ChangeSource where
  toText = \case
    Automatic -> "Automatic"
    DirectModification -> "DirectModification"
    ParameterReference -> "ParameterReference"
    ResourceAttribute -> "ResourceAttribute"
    ResourceReference -> "ResourceReference"

instance Hashable ChangeSource

instance NFData ChangeSource

instance ToByteString ChangeSource

instance ToQuery ChangeSource

instance ToHeader ChangeSource

instance FromXML ChangeSource where
  parseXML = parseXMLText "ChangeSource"
