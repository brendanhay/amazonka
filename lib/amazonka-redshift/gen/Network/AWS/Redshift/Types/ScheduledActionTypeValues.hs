{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionTypeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionTypeValues where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduledActionTypeValues
  = PauseCluster
  | ResizeCluster
  | ResumeCluster
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

instance FromText ScheduledActionTypeValues where
  parser =
    takeLowerText >>= \case
      "pausecluster" -> pure PauseCluster
      "resizecluster" -> pure ResizeCluster
      "resumecluster" -> pure ResumeCluster
      e ->
        fromTextError $
          "Failure parsing ScheduledActionTypeValues from value: '" <> e
            <> "'. Accepted values: pausecluster, resizecluster, resumecluster"

instance ToText ScheduledActionTypeValues where
  toText = \case
    PauseCluster -> "PauseCluster"
    ResizeCluster -> "ResizeCluster"
    ResumeCluster -> "ResumeCluster"

instance Hashable ScheduledActionTypeValues

instance NFData ScheduledActionTypeValues

instance ToByteString ScheduledActionTypeValues

instance ToQuery ScheduledActionTypeValues

instance ToHeader ScheduledActionTypeValues
