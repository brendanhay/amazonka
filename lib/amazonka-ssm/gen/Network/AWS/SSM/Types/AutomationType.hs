{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationType where

import Network.AWS.Prelude

data AutomationType
  = CrossAccount
  | Local
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

instance FromText AutomationType where
  parser =
    takeLowerText >>= \case
      "crossaccount" -> pure CrossAccount
      "local" -> pure Local
      e ->
        fromTextError $
          "Failure parsing AutomationType from value: '" <> e
            <> "'. Accepted values: crossaccount, local"

instance ToText AutomationType where
  toText = \case
    CrossAccount -> "CrossAccount"
    Local -> "Local"

instance Hashable AutomationType

instance NFData AutomationType

instance ToByteString AutomationType

instance ToQuery AutomationType

instance ToHeader AutomationType

instance FromJSON AutomationType where
  parseJSON = parseJSONText "AutomationType"
