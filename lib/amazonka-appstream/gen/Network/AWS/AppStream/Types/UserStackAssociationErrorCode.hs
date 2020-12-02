{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociationErrorCode where

import Network.AWS.Prelude

data UserStackAssociationErrorCode
  = USAECDirectoryNotFound
  | USAECInternalError
  | USAECStackNotFound
  | USAECUserNameNotFound
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

instance FromText UserStackAssociationErrorCode where
  parser =
    takeLowerText >>= \case
      "directory_not_found" -> pure USAECDirectoryNotFound
      "internal_error" -> pure USAECInternalError
      "stack_not_found" -> pure USAECStackNotFound
      "user_name_not_found" -> pure USAECUserNameNotFound
      e ->
        fromTextError $
          "Failure parsing UserStackAssociationErrorCode from value: '" <> e
            <> "'. Accepted values: directory_not_found, internal_error, stack_not_found, user_name_not_found"

instance ToText UserStackAssociationErrorCode where
  toText = \case
    USAECDirectoryNotFound -> "DIRECTORY_NOT_FOUND"
    USAECInternalError -> "INTERNAL_ERROR"
    USAECStackNotFound -> "STACK_NOT_FOUND"
    USAECUserNameNotFound -> "USER_NAME_NOT_FOUND"

instance Hashable UserStackAssociationErrorCode

instance NFData UserStackAssociationErrorCode

instance ToByteString UserStackAssociationErrorCode

instance ToQuery UserStackAssociationErrorCode

instance ToHeader UserStackAssociationErrorCode

instance FromJSON UserStackAssociationErrorCode where
  parseJSON = parseJSONText "UserStackAssociationErrorCode"
