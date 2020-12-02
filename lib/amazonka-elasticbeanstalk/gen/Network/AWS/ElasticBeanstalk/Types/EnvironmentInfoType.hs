{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoType where

import Network.AWS.Prelude

data EnvironmentInfoType
  = Bundle
  | Tail
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

instance FromText EnvironmentInfoType where
  parser =
    takeLowerText >>= \case
      "bundle" -> pure Bundle
      "tail" -> pure Tail
      e ->
        fromTextError $
          "Failure parsing EnvironmentInfoType from value: '" <> e
            <> "'. Accepted values: bundle, tail"

instance ToText EnvironmentInfoType where
  toText = \case
    Bundle -> "bundle"
    Tail -> "tail"

instance Hashable EnvironmentInfoType

instance NFData EnvironmentInfoType

instance ToByteString EnvironmentInfoType

instance ToQuery EnvironmentInfoType

instance ToHeader EnvironmentInfoType

instance FromXML EnvironmentInfoType where
  parseXML = parseXMLText "EnvironmentInfoType"
