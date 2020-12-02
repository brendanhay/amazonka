{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotInstanceInterruptionBehavior
  = SIIBHibernate
  | SIIBStop
  | SIIBTerminate
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

instance FromText SpotInstanceInterruptionBehavior where
  parser =
    takeLowerText >>= \case
      "hibernate" -> pure SIIBHibernate
      "stop" -> pure SIIBStop
      "terminate" -> pure SIIBTerminate
      e ->
        fromTextError $
          "Failure parsing SpotInstanceInterruptionBehavior from value: '" <> e
            <> "'. Accepted values: hibernate, stop, terminate"

instance ToText SpotInstanceInterruptionBehavior where
  toText = \case
    SIIBHibernate -> "hibernate"
    SIIBStop -> "stop"
    SIIBTerminate -> "terminate"

instance Hashable SpotInstanceInterruptionBehavior

instance NFData SpotInstanceInterruptionBehavior

instance ToByteString SpotInstanceInterruptionBehavior

instance ToQuery SpotInstanceInterruptionBehavior

instance ToHeader SpotInstanceInterruptionBehavior

instance FromXML SpotInstanceInterruptionBehavior where
  parseXML = parseXMLText "SpotInstanceInterruptionBehavior"
