{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode where

import Network.AWS.Prelude

data HTTPEndpointS3BackupMode
  = AllData
  | FailedDataOnly
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

instance FromText HTTPEndpointS3BackupMode where
  parser =
    takeLowerText >>= \case
      "alldata" -> pure AllData
      "faileddataonly" -> pure FailedDataOnly
      e ->
        fromTextError $
          "Failure parsing HTTPEndpointS3BackupMode from value: '" <> e
            <> "'. Accepted values: alldata, faileddataonly"

instance ToText HTTPEndpointS3BackupMode where
  toText = \case
    AllData -> "AllData"
    FailedDataOnly -> "FailedDataOnly"

instance Hashable HTTPEndpointS3BackupMode

instance NFData HTTPEndpointS3BackupMode

instance ToByteString HTTPEndpointS3BackupMode

instance ToQuery HTTPEndpointS3BackupMode

instance ToHeader HTTPEndpointS3BackupMode

instance ToJSON HTTPEndpointS3BackupMode where
  toJSON = toJSONText

instance FromJSON HTTPEndpointS3BackupMode where
  parseJSON = parseJSONText "HTTPEndpointS3BackupMode"
