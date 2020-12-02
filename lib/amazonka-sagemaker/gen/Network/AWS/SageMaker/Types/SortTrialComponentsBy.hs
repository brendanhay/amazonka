{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortTrialComponentsBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SortTrialComponentsBy where

import Network.AWS.Prelude

data SortTrialComponentsBy
  = STCBCreationTime
  | STCBName
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

instance FromText SortTrialComponentsBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure STCBCreationTime
      "name" -> pure STCBName
      e ->
        fromTextError $
          "Failure parsing SortTrialComponentsBy from value: '" <> e
            <> "'. Accepted values: creationtime, name"

instance ToText SortTrialComponentsBy where
  toText = \case
    STCBCreationTime -> "CreationTime"
    STCBName -> "Name"

instance Hashable SortTrialComponentsBy

instance NFData SortTrialComponentsBy

instance ToByteString SortTrialComponentsBy

instance ToQuery SortTrialComponentsBy

instance ToHeader SortTrialComponentsBy

instance ToJSON SortTrialComponentsBy where
  toJSON = toJSONText
