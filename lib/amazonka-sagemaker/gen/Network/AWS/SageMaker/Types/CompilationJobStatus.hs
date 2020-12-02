{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CompilationJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CompilationJobStatus where

import Network.AWS.Prelude

data CompilationJobStatus
  = CJSCompleted
  | CJSFailed
  | CJSInprogress
  | CJSStarting
  | CJSStopped
  | CJSStopping
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

instance FromText CompilationJobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure CJSCompleted
      "failed" -> pure CJSFailed
      "inprogress" -> pure CJSInprogress
      "starting" -> pure CJSStarting
      "stopped" -> pure CJSStopped
      "stopping" -> pure CJSStopping
      e ->
        fromTextError $
          "Failure parsing CompilationJobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, starting, stopped, stopping"

instance ToText CompilationJobStatus where
  toText = \case
    CJSCompleted -> "COMPLETED"
    CJSFailed -> "FAILED"
    CJSInprogress -> "INPROGRESS"
    CJSStarting -> "STARTING"
    CJSStopped -> "STOPPED"
    CJSStopping -> "STOPPING"

instance Hashable CompilationJobStatus

instance NFData CompilationJobStatus

instance ToByteString CompilationJobStatus

instance ToQuery CompilationJobStatus

instance ToHeader CompilationJobStatus

instance ToJSON CompilationJobStatus where
  toJSON = toJSONText

instance FromJSON CompilationJobStatus where
  parseJSON = parseJSONText "CompilationJobStatus"
