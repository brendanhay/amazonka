{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RequestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RequestStatus where

import Network.AWS.Prelude

data RequestStatus
  = Available
  | Creating
  | Failed
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

instance FromText RequestStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "creating" -> pure Creating
      "failed" -> pure Failed
      e ->
        fromTextError $
          "Failure parsing RequestStatus from value: '" <> e
            <> "'. Accepted values: available, creating, failed"

instance ToText RequestStatus where
  toText = \case
    Available -> "AVAILABLE"
    Creating -> "CREATING"
    Failed -> "FAILED"

instance Hashable RequestStatus

instance NFData RequestStatus

instance ToByteString RequestStatus

instance ToQuery RequestStatus

instance ToHeader RequestStatus

instance FromJSON RequestStatus where
  parseJSON = parseJSONText "RequestStatus"
