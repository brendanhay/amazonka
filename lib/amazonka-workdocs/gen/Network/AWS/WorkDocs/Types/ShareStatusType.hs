{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ShareStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ShareStatusType where

import Network.AWS.Prelude

data ShareStatusType
  = Failure
  | Success
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

instance FromText ShareStatusType where
  parser =
    takeLowerText >>= \case
      "failure" -> pure Failure
      "success" -> pure Success
      e ->
        fromTextError $
          "Failure parsing ShareStatusType from value: '" <> e
            <> "'. Accepted values: failure, success"

instance ToText ShareStatusType where
  toText = \case
    Failure -> "FAILURE"
    Success -> "SUCCESS"

instance Hashable ShareStatusType

instance NFData ShareStatusType

instance ToByteString ShareStatusType

instance ToQuery ShareStatusType

instance ToHeader ShareStatusType

instance FromJSON ShareStatusType where
  parseJSON = parseJSONText "ShareStatusType"
