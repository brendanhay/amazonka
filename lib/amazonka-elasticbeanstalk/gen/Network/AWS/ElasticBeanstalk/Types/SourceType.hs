{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceType where

import Network.AWS.Prelude

data SourceType
  = Git
  | Zip
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

instance FromText SourceType where
  parser =
    takeLowerText >>= \case
      "git" -> pure Git
      "zip" -> pure Zip
      e ->
        fromTextError $
          "Failure parsing SourceType from value: '" <> e
            <> "'. Accepted values: git, zip"

instance ToText SourceType where
  toText = \case
    Git -> "Git"
    Zip -> "Zip"

instance Hashable SourceType

instance NFData SourceType

instance ToByteString SourceType

instance ToQuery SourceType

instance ToHeader SourceType

instance FromXML SourceType where
  parseXML = parseXMLText "SourceType"
