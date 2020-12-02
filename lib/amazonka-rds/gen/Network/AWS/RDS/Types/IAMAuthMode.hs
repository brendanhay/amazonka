{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.IAMAuthMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.IAMAuthMode where

import Network.AWS.Prelude

data IAMAuthMode
  = IAMDisabled
  | IAMRequired
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

instance FromText IAMAuthMode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure IAMDisabled
      "required" -> pure IAMRequired
      e ->
        fromTextError $
          "Failure parsing IAMAuthMode from value: '" <> e
            <> "'. Accepted values: disabled, required"

instance ToText IAMAuthMode where
  toText = \case
    IAMDisabled -> "DISABLED"
    IAMRequired -> "REQUIRED"

instance Hashable IAMAuthMode

instance NFData IAMAuthMode

instance ToByteString IAMAuthMode

instance ToQuery IAMAuthMode

instance ToHeader IAMAuthMode

instance FromXML IAMAuthMode where
  parseXML = parseXMLText "IAMAuthMode"
