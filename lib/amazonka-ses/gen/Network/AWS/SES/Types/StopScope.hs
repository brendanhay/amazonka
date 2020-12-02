{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.StopScope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.StopScope where

import Network.AWS.Prelude

data StopScope = RuleSet
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

instance FromText StopScope where
  parser =
    takeLowerText >>= \case
      "ruleset" -> pure RuleSet
      e ->
        fromTextError $
          "Failure parsing StopScope from value: '" <> e
            <> "'. Accepted values: ruleset"

instance ToText StopScope where
  toText = \case
    RuleSet -> "RuleSet"

instance Hashable StopScope

instance NFData StopScope

instance ToByteString StopScope

instance ToQuery StopScope

instance ToHeader StopScope

instance FromXML StopScope where
  parseXML = parseXMLText "StopScope"
