{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationStrategy where

import Network.AWS.Prelude

data ServerValidationStrategy = Userdata
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

instance FromText ServerValidationStrategy where
  parser =
    takeLowerText >>= \case
      "userdata" -> pure Userdata
      e ->
        fromTextError $
          "Failure parsing ServerValidationStrategy from value: '" <> e
            <> "'. Accepted values: userdata"

instance ToText ServerValidationStrategy where
  toText = \case
    Userdata -> "USERDATA"

instance Hashable ServerValidationStrategy

instance NFData ServerValidationStrategy

instance ToByteString ServerValidationStrategy

instance ToQuery ServerValidationStrategy

instance ToHeader ServerValidationStrategy

instance ToJSON ServerValidationStrategy where
  toJSON = toJSONText

instance FromJSON ServerValidationStrategy where
  parseJSON = parseJSONText "ServerValidationStrategy"
