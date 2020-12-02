{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SlaMet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SlaMet where

import Network.AWS.Prelude

data SlaMet
  = NO
  | Na
  | Yes
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

instance FromText SlaMet where
  parser =
    takeLowerText >>= \case
      "no" -> pure NO
      "n/a" -> pure Na
      "yes" -> pure Yes
      e ->
        fromTextError $
          "Failure parsing SlaMet from value: '" <> e
            <> "'. Accepted values: no, n/a, yes"

instance ToText SlaMet where
  toText = \case
    NO -> "no"
    Na -> "n/a"
    Yes -> "yes"

instance Hashable SlaMet

instance NFData SlaMet

instance ToByteString SlaMet

instance ToQuery SlaMet

instance ToHeader SlaMet

instance FromXML SlaMet where
  parseXML = parseXMLText "SlaMet"
