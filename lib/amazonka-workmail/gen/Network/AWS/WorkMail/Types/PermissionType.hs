{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.PermissionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.PermissionType where

import Network.AWS.Prelude

data PermissionType
  = FullAccess
  | SendAs
  | SendOnBehalf
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

instance FromText PermissionType where
  parser =
    takeLowerText >>= \case
      "full_access" -> pure FullAccess
      "send_as" -> pure SendAs
      "send_on_behalf" -> pure SendOnBehalf
      e ->
        fromTextError $
          "Failure parsing PermissionType from value: '" <> e
            <> "'. Accepted values: full_access, send_as, send_on_behalf"

instance ToText PermissionType where
  toText = \case
    FullAccess -> "FULL_ACCESS"
    SendAs -> "SEND_AS"
    SendOnBehalf -> "SEND_ON_BEHALF"

instance Hashable PermissionType

instance NFData PermissionType

instance ToByteString PermissionType

instance ToQuery PermissionType

instance ToHeader PermissionType

instance ToJSON PermissionType where
  toJSON = toJSONText

instance FromJSON PermissionType where
  parseJSON = parseJSONText "PermissionType"
