{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AuthScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AuthScheme where

import Network.AWS.Prelude

data AuthScheme = Secrets
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

instance FromText AuthScheme where
  parser =
    takeLowerText >>= \case
      "secrets" -> pure Secrets
      e ->
        fromTextError $
          "Failure parsing AuthScheme from value: '" <> e
            <> "'. Accepted values: secrets"

instance ToText AuthScheme where
  toText = \case
    Secrets -> "SECRETS"

instance Hashable AuthScheme

instance NFData AuthScheme

instance ToByteString AuthScheme

instance ToQuery AuthScheme

instance ToHeader AuthScheme

instance FromXML AuthScheme where
  parseXML = parseXMLText "AuthScheme"
