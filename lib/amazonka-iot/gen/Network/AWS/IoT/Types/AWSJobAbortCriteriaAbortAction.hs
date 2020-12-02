{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortCriteriaAbortAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortCriteriaAbortAction where

import Network.AWS.Prelude

data AWSJobAbortCriteriaAbortAction = Cancel
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

instance FromText AWSJobAbortCriteriaAbortAction where
  parser =
    takeLowerText >>= \case
      "cancel" -> pure Cancel
      e ->
        fromTextError $
          "Failure parsing AWSJobAbortCriteriaAbortAction from value: '" <> e
            <> "'. Accepted values: cancel"

instance ToText AWSJobAbortCriteriaAbortAction where
  toText = \case
    Cancel -> "CANCEL"

instance Hashable AWSJobAbortCriteriaAbortAction

instance NFData AWSJobAbortCriteriaAbortAction

instance ToByteString AWSJobAbortCriteriaAbortAction

instance ToQuery AWSJobAbortCriteriaAbortAction

instance ToHeader AWSJobAbortCriteriaAbortAction

instance ToJSON AWSJobAbortCriteriaAbortAction where
  toJSON = toJSONText
