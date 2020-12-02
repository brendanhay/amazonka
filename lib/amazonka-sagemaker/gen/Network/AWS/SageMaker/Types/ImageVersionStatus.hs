{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionStatus where

import Network.AWS.Prelude

data ImageVersionStatus
  = IVSCreateFailed
  | IVSCreated
  | IVSCreating
  | IVSDeleteFailed
  | IVSDeleting
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

instance FromText ImageVersionStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure IVSCreateFailed
      "created" -> pure IVSCreated
      "creating" -> pure IVSCreating
      "delete_failed" -> pure IVSDeleteFailed
      "deleting" -> pure IVSDeleting
      e ->
        fromTextError $
          "Failure parsing ImageVersionStatus from value: '" <> e
            <> "'. Accepted values: create_failed, created, creating, delete_failed, deleting"

instance ToText ImageVersionStatus where
  toText = \case
    IVSCreateFailed -> "CREATE_FAILED"
    IVSCreated -> "CREATED"
    IVSCreating -> "CREATING"
    IVSDeleteFailed -> "DELETE_FAILED"
    IVSDeleting -> "DELETING"

instance Hashable ImageVersionStatus

instance NFData ImageVersionStatus

instance ToByteString ImageVersionStatus

instance ToQuery ImageVersionStatus

instance ToHeader ImageVersionStatus

instance FromJSON ImageVersionStatus where
  parseJSON = parseJSONText "ImageVersionStatus"
