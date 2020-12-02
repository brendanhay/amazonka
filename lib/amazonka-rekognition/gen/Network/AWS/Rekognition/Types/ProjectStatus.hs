{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectStatus where

import Network.AWS.Prelude

data ProjectStatus
  = PSCreated
  | PSCreating
  | PSDeleting
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

instance FromText ProjectStatus where
  parser =
    takeLowerText >>= \case
      "created" -> pure PSCreated
      "creating" -> pure PSCreating
      "deleting" -> pure PSDeleting
      e ->
        fromTextError $
          "Failure parsing ProjectStatus from value: '" <> e
            <> "'. Accepted values: created, creating, deleting"

instance ToText ProjectStatus where
  toText = \case
    PSCreated -> "CREATED"
    PSCreating -> "CREATING"
    PSDeleting -> "DELETING"

instance Hashable ProjectStatus

instance NFData ProjectStatus

instance ToByteString ProjectStatus

instance ToQuery ProjectStatus

instance ToHeader ProjectStatus

instance FromJSON ProjectStatus where
  parseJSON = parseJSONText "ProjectStatus"
