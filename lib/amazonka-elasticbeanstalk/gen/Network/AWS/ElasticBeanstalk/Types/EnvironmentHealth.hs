{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealth where

import Network.AWS.Prelude

data EnvironmentHealth
  = Green
  | Grey
  | Red
  | Yellow
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

instance FromText EnvironmentHealth where
  parser =
    takeLowerText >>= \case
      "green" -> pure Green
      "grey" -> pure Grey
      "red" -> pure Red
      "yellow" -> pure Yellow
      e ->
        fromTextError $
          "Failure parsing EnvironmentHealth from value: '" <> e
            <> "'. Accepted values: green, grey, red, yellow"

instance ToText EnvironmentHealth where
  toText = \case
    Green -> "Green"
    Grey -> "Grey"
    Red -> "Red"
    Yellow -> "Yellow"

instance Hashable EnvironmentHealth

instance NFData EnvironmentHealth

instance ToByteString EnvironmentHealth

instance ToQuery EnvironmentHealth

instance ToHeader EnvironmentHealth

instance FromXML EnvironmentHealth where
  parseXML = parseXMLText "EnvironmentHealth"
