{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContainerMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContainerMode where

import Network.AWS.Prelude

data ContainerMode
  = MultiModel
  | SingleModel
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

instance FromText ContainerMode where
  parser =
    takeLowerText >>= \case
      "multimodel" -> pure MultiModel
      "singlemodel" -> pure SingleModel
      e ->
        fromTextError $
          "Failure parsing ContainerMode from value: '" <> e
            <> "'. Accepted values: multimodel, singlemodel"

instance ToText ContainerMode where
  toText = \case
    MultiModel -> "MultiModel"
    SingleModel -> "SingleModel"

instance Hashable ContainerMode

instance NFData ContainerMode

instance ToByteString ContainerMode

instance ToQuery ContainerMode

instance ToHeader ContainerMode

instance ToJSON ContainerMode where
  toJSON = toJSONText

instance FromJSON ContainerMode where
  parseJSON = parseJSONText "ContainerMode"
