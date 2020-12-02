{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.PreviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.PreviewStatus where

import Network.AWS.Prelude

data PreviewStatus
  = PSCompleted
  | PSWorkInProgress
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

instance FromText PreviewStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure PSCompleted
      "work_in_progress" -> pure PSWorkInProgress
      e ->
        fromTextError $
          "Failure parsing PreviewStatus from value: '" <> e
            <> "'. Accepted values: completed, work_in_progress"

instance ToText PreviewStatus where
  toText = \case
    PSCompleted -> "COMPLETED"
    PSWorkInProgress -> "WORK_IN_PROGRESS"

instance Hashable PreviewStatus

instance NFData PreviewStatus

instance ToByteString PreviewStatus

instance ToQuery PreviewStatus

instance ToHeader PreviewStatus

instance FromJSON PreviewStatus where
  parseJSON = parseJSONText "PreviewStatus"
