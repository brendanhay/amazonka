{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RootAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RootAccess where

import Network.AWS.Prelude

data RootAccess
  = RADisabled
  | RAEnabled
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

instance FromText RootAccess where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure RADisabled
      "enabled" -> pure RAEnabled
      e ->
        fromTextError $
          "Failure parsing RootAccess from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText RootAccess where
  toText = \case
    RADisabled -> "Disabled"
    RAEnabled -> "Enabled"

instance Hashable RootAccess

instance NFData RootAccess

instance ToByteString RootAccess

instance ToQuery RootAccess

instance ToHeader RootAccess

instance ToJSON RootAccess where
  toJSON = toJSONText

instance FromJSON RootAccess where
  parseJSON = parseJSONText "RootAccess"
