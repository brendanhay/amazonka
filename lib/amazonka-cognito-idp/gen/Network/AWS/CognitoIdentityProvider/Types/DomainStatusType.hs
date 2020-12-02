{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DomainStatusType where

import Network.AWS.Prelude

data DomainStatusType
  = DSTActive
  | DSTCreating
  | DSTDeleting
  | DSTFailed
  | DSTUpdating
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

instance FromText DomainStatusType where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSTActive
      "creating" -> pure DSTCreating
      "deleting" -> pure DSTDeleting
      "failed" -> pure DSTFailed
      "updating" -> pure DSTUpdating
      e ->
        fromTextError $
          "Failure parsing DomainStatusType from value: '" <> e
            <> "'. Accepted values: active, creating, deleting, failed, updating"

instance ToText DomainStatusType where
  toText = \case
    DSTActive -> "ACTIVE"
    DSTCreating -> "CREATING"
    DSTDeleting -> "DELETING"
    DSTFailed -> "FAILED"
    DSTUpdating -> "UPDATING"

instance Hashable DomainStatusType

instance NFData DomainStatusType

instance ToByteString DomainStatusType

instance ToQuery DomainStatusType

instance ToHeader DomainStatusType

instance FromJSON DomainStatusType where
  parseJSON = parseJSONText "DomainStatusType"
