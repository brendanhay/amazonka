{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DataSourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DataSourceStatus where

import Network.AWS.Prelude

data DataSourceStatus
  = DSSDisabled
  | DSSEnabled
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

instance FromText DataSourceStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DSSDisabled
      "enabled" -> pure DSSEnabled
      e ->
        fromTextError $
          "Failure parsing DataSourceStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DataSourceStatus where
  toText = \case
    DSSDisabled -> "DISABLED"
    DSSEnabled -> "ENABLED"

instance Hashable DataSourceStatus

instance NFData DataSourceStatus

instance ToByteString DataSourceStatus

instance ToQuery DataSourceStatus

instance ToHeader DataSourceStatus

instance FromJSON DataSourceStatus where
  parseJSON = parseJSONText "DataSourceStatus"
