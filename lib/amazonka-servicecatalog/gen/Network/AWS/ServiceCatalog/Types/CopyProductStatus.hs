{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.CopyProductStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CopyProductStatus where

import Network.AWS.Prelude

data CopyProductStatus
  = CPSFailed
  | CPSInProgress
  | CPSSucceeded
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

instance FromText CopyProductStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure CPSFailed
      "in_progress" -> pure CPSInProgress
      "succeeded" -> pure CPSSucceeded
      e ->
        fromTextError $
          "Failure parsing CopyProductStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText CopyProductStatus where
  toText = \case
    CPSFailed -> "FAILED"
    CPSInProgress -> "IN_PROGRESS"
    CPSSucceeded -> "SUCCEEDED"

instance Hashable CopyProductStatus

instance NFData CopyProductStatus

instance ToByteString CopyProductStatus

instance ToQuery CopyProductStatus

instance ToHeader CopyProductStatus

instance FromJSON CopyProductStatus where
  parseJSON = parseJSONText "CopyProductStatus"
