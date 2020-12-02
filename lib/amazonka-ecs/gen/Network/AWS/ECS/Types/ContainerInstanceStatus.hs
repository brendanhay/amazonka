{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerInstanceStatus where

import Network.AWS.Prelude

data ContainerInstanceStatus
  = Active
  | Deregistering
  | Draining
  | Registering
  | RegistrationFailed
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

instance FromText ContainerInstanceStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deregistering" -> pure Deregistering
      "draining" -> pure Draining
      "registering" -> pure Registering
      "registration_failed" -> pure RegistrationFailed
      e ->
        fromTextError $
          "Failure parsing ContainerInstanceStatus from value: '" <> e
            <> "'. Accepted values: active, deregistering, draining, registering, registration_failed"

instance ToText ContainerInstanceStatus where
  toText = \case
    Active -> "ACTIVE"
    Deregistering -> "DEREGISTERING"
    Draining -> "DRAINING"
    Registering -> "REGISTERING"
    RegistrationFailed -> "REGISTRATION_FAILED"

instance Hashable ContainerInstanceStatus

instance NFData ContainerInstanceStatus

instance ToByteString ContainerInstanceStatus

instance ToQuery ContainerInstanceStatus

instance ToHeader ContainerInstanceStatus

instance ToJSON ContainerInstanceStatus where
  toJSON = toJSONText
