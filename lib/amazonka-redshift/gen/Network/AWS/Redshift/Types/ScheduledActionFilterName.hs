{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionFilterName where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduledActionFilterName
  = ClusterIdentifier
  | IAMRole
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

instance FromText ScheduledActionFilterName where
  parser =
    takeLowerText >>= \case
      "cluster-identifier" -> pure ClusterIdentifier
      "iam-role" -> pure IAMRole
      e ->
        fromTextError $
          "Failure parsing ScheduledActionFilterName from value: '" <> e
            <> "'. Accepted values: cluster-identifier, iam-role"

instance ToText ScheduledActionFilterName where
  toText = \case
    ClusterIdentifier -> "cluster-identifier"
    IAMRole -> "iam-role"

instance Hashable ScheduledActionFilterName

instance NFData ScheduledActionFilterName

instance ToByteString ScheduledActionFilterName

instance ToQuery ScheduledActionFilterName

instance ToHeader ScheduledActionFilterName
