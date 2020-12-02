{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureStatus where

import Network.AWS.Prelude

data CaptureStatus
  = CStarted
  | CStopped
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

instance FromText CaptureStatus where
  parser =
    takeLowerText >>= \case
      "started" -> pure CStarted
      "stopped" -> pure CStopped
      e ->
        fromTextError $
          "Failure parsing CaptureStatus from value: '" <> e
            <> "'. Accepted values: started, stopped"

instance ToText CaptureStatus where
  toText = \case
    CStarted -> "Started"
    CStopped -> "Stopped"

instance Hashable CaptureStatus

instance NFData CaptureStatus

instance ToByteString CaptureStatus

instance ToQuery CaptureStatus

instance ToHeader CaptureStatus

instance FromJSON CaptureStatus where
  parseJSON = parseJSONText "CaptureStatus"
