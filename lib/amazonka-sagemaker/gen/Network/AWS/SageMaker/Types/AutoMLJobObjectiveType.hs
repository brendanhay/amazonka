{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobObjectiveType where

import Network.AWS.Prelude

data AutoMLJobObjectiveType
  = AMLJOTMaximize
  | AMLJOTMinimize
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

instance FromText AutoMLJobObjectiveType where
  parser =
    takeLowerText >>= \case
      "maximize" -> pure AMLJOTMaximize
      "minimize" -> pure AMLJOTMinimize
      e ->
        fromTextError $
          "Failure parsing AutoMLJobObjectiveType from value: '" <> e
            <> "'. Accepted values: maximize, minimize"

instance ToText AutoMLJobObjectiveType where
  toText = \case
    AMLJOTMaximize -> "Maximize"
    AMLJOTMinimize -> "Minimize"

instance Hashable AutoMLJobObjectiveType

instance NFData AutoMLJobObjectiveType

instance ToByteString AutoMLJobObjectiveType

instance ToQuery AutoMLJobObjectiveType

instance ToHeader AutoMLJobObjectiveType

instance FromJSON AutoMLJobObjectiveType where
  parseJSON = parseJSONText "AutoMLJobObjectiveType"
