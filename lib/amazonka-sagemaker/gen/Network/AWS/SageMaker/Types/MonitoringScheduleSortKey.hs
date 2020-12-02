{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleSortKey where

import Network.AWS.Prelude

data MonitoringScheduleSortKey
  = CreationTime
  | Name
  | Status
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

instance FromText MonitoringScheduleSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure CreationTime
      "name" -> pure Name
      "status" -> pure Status
      e ->
        fromTextError $
          "Failure parsing MonitoringScheduleSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText MonitoringScheduleSortKey where
  toText = \case
    CreationTime -> "CreationTime"
    Name -> "Name"
    Status -> "Status"

instance Hashable MonitoringScheduleSortKey

instance NFData MonitoringScheduleSortKey

instance ToByteString MonitoringScheduleSortKey

instance ToQuery MonitoringScheduleSortKey

instance ToHeader MonitoringScheduleSortKey

instance ToJSON MonitoringScheduleSortKey where
  toJSON = toJSONText
