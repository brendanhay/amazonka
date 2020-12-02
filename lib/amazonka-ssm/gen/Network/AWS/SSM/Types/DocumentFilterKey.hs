{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFilterKey where

import Network.AWS.Prelude

data DocumentFilterKey
  = DFKDocumentType
  | DFKName
  | DFKOwner
  | DFKPlatformTypes
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

instance FromText DocumentFilterKey where
  parser =
    takeLowerText >>= \case
      "documenttype" -> pure DFKDocumentType
      "name" -> pure DFKName
      "owner" -> pure DFKOwner
      "platformtypes" -> pure DFKPlatformTypes
      e ->
        fromTextError $
          "Failure parsing DocumentFilterKey from value: '" <> e
            <> "'. Accepted values: documenttype, name, owner, platformtypes"

instance ToText DocumentFilterKey where
  toText = \case
    DFKDocumentType -> "DocumentType"
    DFKName -> "Name"
    DFKOwner -> "Owner"
    DFKPlatformTypes -> "PlatformTypes"

instance Hashable DocumentFilterKey

instance NFData DocumentFilterKey

instance ToByteString DocumentFilterKey

instance ToQuery DocumentFilterKey

instance ToHeader DocumentFilterKey

instance ToJSON DocumentFilterKey where
  toJSON = toJSONText
