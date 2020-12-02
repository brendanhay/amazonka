{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceInterruptionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceInterruptionBehavior where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceInterruptionBehavior
  = Hibernate
  | Stop
  | Terminate
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

instance FromText InstanceInterruptionBehavior where
  parser =
    takeLowerText >>= \case
      "hibernate" -> pure Hibernate
      "stop" -> pure Stop
      "terminate" -> pure Terminate
      e ->
        fromTextError $
          "Failure parsing InstanceInterruptionBehavior from value: '" <> e
            <> "'. Accepted values: hibernate, stop, terminate"

instance ToText InstanceInterruptionBehavior where
  toText = \case
    Hibernate -> "hibernate"
    Stop -> "stop"
    Terminate -> "terminate"

instance Hashable InstanceInterruptionBehavior

instance NFData InstanceInterruptionBehavior

instance ToByteString InstanceInterruptionBehavior

instance ToQuery InstanceInterruptionBehavior

instance ToHeader InstanceInterruptionBehavior

instance FromXML InstanceInterruptionBehavior where
  parseXML = parseXMLText "InstanceInterruptionBehavior"
