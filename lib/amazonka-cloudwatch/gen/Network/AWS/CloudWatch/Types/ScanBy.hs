{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.ScanBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.ScanBy where

import Network.AWS.Prelude

data ScanBy
  = TimestampAscending
  | TimestampDescending
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

instance FromText ScanBy where
  parser =
    takeLowerText >>= \case
      "timestampascending" -> pure TimestampAscending
      "timestampdescending" -> pure TimestampDescending
      e ->
        fromTextError $
          "Failure parsing ScanBy from value: '" <> e
            <> "'. Accepted values: timestampascending, timestampdescending"

instance ToText ScanBy where
  toText = \case
    TimestampAscending -> "TimestampAscending"
    TimestampDescending -> "TimestampDescending"

instance Hashable ScanBy

instance NFData ScanBy

instance ToByteString ScanBy

instance ToQuery ScanBy

instance ToHeader ScanBy
