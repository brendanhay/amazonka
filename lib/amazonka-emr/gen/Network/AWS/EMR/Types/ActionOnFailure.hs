{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ActionOnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ActionOnFailure where

import Network.AWS.Prelude

data ActionOnFailure
  = CancelAndWait
  | Continue
  | TerminateCluster
  | TerminateJobFlow
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

instance FromText ActionOnFailure where
  parser =
    takeLowerText >>= \case
      "cancel_and_wait" -> pure CancelAndWait
      "continue" -> pure Continue
      "terminate_cluster" -> pure TerminateCluster
      "terminate_job_flow" -> pure TerminateJobFlow
      e ->
        fromTextError $
          "Failure parsing ActionOnFailure from value: '" <> e
            <> "'. Accepted values: cancel_and_wait, continue, terminate_cluster, terminate_job_flow"

instance ToText ActionOnFailure where
  toText = \case
    CancelAndWait -> "CANCEL_AND_WAIT"
    Continue -> "CONTINUE"
    TerminateCluster -> "TERMINATE_CLUSTER"
    TerminateJobFlow -> "TERMINATE_JOB_FLOW"

instance Hashable ActionOnFailure

instance NFData ActionOnFailure

instance ToByteString ActionOnFailure

instance ToQuery ActionOnFailure

instance ToHeader ActionOnFailure

instance ToJSON ActionOnFailure where
  toJSON = toJSONText

instance FromJSON ActionOnFailure where
  parseJSON = parseJSONText "ActionOnFailure"
