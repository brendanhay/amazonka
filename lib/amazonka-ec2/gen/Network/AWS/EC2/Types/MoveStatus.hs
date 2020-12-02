{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MoveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MoveStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MoveStatus
  = MovingToVPC
  | RestoringToClassic
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

instance FromText MoveStatus where
  parser =
    takeLowerText >>= \case
      "movingtovpc" -> pure MovingToVPC
      "restoringtoclassic" -> pure RestoringToClassic
      e ->
        fromTextError $
          "Failure parsing MoveStatus from value: '" <> e
            <> "'. Accepted values: movingtovpc, restoringtoclassic"

instance ToText MoveStatus where
  toText = \case
    MovingToVPC -> "movingToVpc"
    RestoringToClassic -> "restoringToClassic"

instance Hashable MoveStatus

instance NFData MoveStatus

instance ToByteString MoveStatus

instance ToQuery MoveStatus

instance ToHeader MoveStatus

instance FromXML MoveStatus where
  parseXML = parseXMLText "MoveStatus"
