{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReleaseStatusValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReleaseStatusValues where

import Network.AWS.Prelude

data ReleaseStatusValues = Beta
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

instance FromText ReleaseStatusValues where
  parser =
    takeLowerText >>= \case
      "beta" -> pure Beta
      e ->
        fromTextError $
          "Failure parsing ReleaseStatusValues from value: '" <> e
            <> "'. Accepted values: beta"

instance ToText ReleaseStatusValues where
  toText = \case
    Beta -> "beta"

instance Hashable ReleaseStatusValues

instance NFData ReleaseStatusValues

instance ToByteString ReleaseStatusValues

instance ToQuery ReleaseStatusValues

instance ToHeader ReleaseStatusValues

instance FromJSON ReleaseStatusValues where
  parseJSON = parseJSONText "ReleaseStatusValues"
