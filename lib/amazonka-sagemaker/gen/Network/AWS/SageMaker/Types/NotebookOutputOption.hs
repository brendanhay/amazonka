{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookOutputOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookOutputOption where

import Network.AWS.Prelude

data NotebookOutputOption
  = Allowed
  | Disabled
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

instance FromText NotebookOutputOption where
  parser =
    takeLowerText >>= \case
      "allowed" -> pure Allowed
      "disabled" -> pure Disabled
      e ->
        fromTextError $
          "Failure parsing NotebookOutputOption from value: '" <> e
            <> "'. Accepted values: allowed, disabled"

instance ToText NotebookOutputOption where
  toText = \case
    Allowed -> "Allowed"
    Disabled -> "Disabled"

instance Hashable NotebookOutputOption

instance NFData NotebookOutputOption

instance ToByteString NotebookOutputOption

instance ToQuery NotebookOutputOption

instance ToHeader NotebookOutputOption

instance ToJSON NotebookOutputOption where
  toJSON = toJSONText

instance FromJSON NotebookOutputOption where
  parseJSON = parseJSONText "NotebookOutputOption"
