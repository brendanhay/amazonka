{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.DirectoryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.DirectoryType where

import Network.AWS.Prelude

data DirectoryType
  = ConnectManaged
  | ExistingDirectory
  | Saml
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

instance FromText DirectoryType where
  parser =
    takeLowerText >>= \case
      "connect_managed" -> pure ConnectManaged
      "existing_directory" -> pure ExistingDirectory
      "saml" -> pure Saml
      e ->
        fromTextError $
          "Failure parsing DirectoryType from value: '" <> e
            <> "'. Accepted values: connect_managed, existing_directory, saml"

instance ToText DirectoryType where
  toText = \case
    ConnectManaged -> "CONNECT_MANAGED"
    ExistingDirectory -> "EXISTING_DIRECTORY"
    Saml -> "SAML"

instance Hashable DirectoryType

instance NFData DirectoryType

instance ToByteString DirectoryType

instance ToQuery DirectoryType

instance ToHeader DirectoryType

instance ToJSON DirectoryType where
  toJSON = toJSONText

instance FromJSON DirectoryType where
  parseJSON = parseJSONText "DirectoryType"
