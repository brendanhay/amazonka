{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContinuousBackupsStatus where

import Network.AWS.Prelude

data ContinuousBackupsStatus
  = CBSDisabled
  | CBSEnabled
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

instance FromText ContinuousBackupsStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CBSDisabled
      "enabled" -> pure CBSEnabled
      e ->
        fromTextError $
          "Failure parsing ContinuousBackupsStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ContinuousBackupsStatus where
  toText = \case
    CBSDisabled -> "DISABLED"
    CBSEnabled -> "ENABLED"

instance Hashable ContinuousBackupsStatus

instance NFData ContinuousBackupsStatus

instance ToByteString ContinuousBackupsStatus

instance ToQuery ContinuousBackupsStatus

instance ToHeader ContinuousBackupsStatus

instance FromJSON ContinuousBackupsStatus where
  parseJSON = parseJSONText "ContinuousBackupsStatus"
