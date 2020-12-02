{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiStatus where

import Network.AWS.Prelude

data HumanTaskUiStatus
  = HTUSActive
  | HTUSDeleting
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

instance FromText HumanTaskUiStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure HTUSActive
      "deleting" -> pure HTUSDeleting
      e ->
        fromTextError $
          "Failure parsing HumanTaskUiStatus from value: '" <> e
            <> "'. Accepted values: active, deleting"

instance ToText HumanTaskUiStatus where
  toText = \case
    HTUSActive -> "Active"
    HTUSDeleting -> "Deleting"

instance Hashable HumanTaskUiStatus

instance NFData HumanTaskUiStatus

instance ToByteString HumanTaskUiStatus

instance ToQuery HumanTaskUiStatus

instance ToHeader HumanTaskUiStatus

instance FromJSON HumanTaskUiStatus where
  parseJSON = parseJSONText "HumanTaskUiStatus"
