{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetType where

import Network.AWS.Prelude

data ChangeSetType
  = CSTCreate
  | CSTImport
  | CSTUpdate
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

instance FromText ChangeSetType where
  parser =
    takeLowerText >>= \case
      "create" -> pure CSTCreate
      "import" -> pure CSTImport
      "update" -> pure CSTUpdate
      e ->
        fromTextError $
          "Failure parsing ChangeSetType from value: '" <> e
            <> "'. Accepted values: create, import, update"

instance ToText ChangeSetType where
  toText = \case
    CSTCreate -> "CREATE"
    CSTImport -> "IMPORT"
    CSTUpdate -> "UPDATE"

instance Hashable ChangeSetType

instance NFData ChangeSetType

instance ToByteString ChangeSetType

instance ToQuery ChangeSetType

instance ToHeader ChangeSetType
