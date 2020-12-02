{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ShutdownBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ShutdownBehavior where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ShutdownBehavior
  = SBStop
  | SBTerminate
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

instance FromText ShutdownBehavior where
  parser =
    takeLowerText >>= \case
      "stop" -> pure SBStop
      "terminate" -> pure SBTerminate
      e ->
        fromTextError $
          "Failure parsing ShutdownBehavior from value: '" <> e
            <> "'. Accepted values: stop, terminate"

instance ToText ShutdownBehavior where
  toText = \case
    SBStop -> "stop"
    SBTerminate -> "terminate"

instance Hashable ShutdownBehavior

instance NFData ShutdownBehavior

instance ToByteString ShutdownBehavior

instance ToQuery ShutdownBehavior

instance ToHeader ShutdownBehavior

instance FromXML ShutdownBehavior where
  parseXML = parseXMLText "ShutdownBehavior"
