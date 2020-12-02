{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScaleDownBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScaleDownBehavior where

import Network.AWS.Prelude

data ScaleDownBehavior
  = TerminateAtInstanceHour
  | TerminateAtTaskCompletion
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

instance FromText ScaleDownBehavior where
  parser =
    takeLowerText >>= \case
      "terminate_at_instance_hour" -> pure TerminateAtInstanceHour
      "terminate_at_task_completion" -> pure TerminateAtTaskCompletion
      e ->
        fromTextError $
          "Failure parsing ScaleDownBehavior from value: '" <> e
            <> "'. Accepted values: terminate_at_instance_hour, terminate_at_task_completion"

instance ToText ScaleDownBehavior where
  toText = \case
    TerminateAtInstanceHour -> "TERMINATE_AT_INSTANCE_HOUR"
    TerminateAtTaskCompletion -> "TERMINATE_AT_TASK_COMPLETION"

instance Hashable ScaleDownBehavior

instance NFData ScaleDownBehavior

instance ToByteString ScaleDownBehavior

instance ToQuery ScaleDownBehavior

instance ToHeader ScaleDownBehavior

instance ToJSON ScaleDownBehavior where
  toJSON = toJSONText

instance FromJSON ScaleDownBehavior where
  parseJSON = parseJSONText "ScaleDownBehavior"
