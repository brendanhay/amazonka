{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.RespondToAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.RespondToAfd where

import Network.AWS.Prelude

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
data RespondToAfd
  = RTANone
  | RTAPassthrough
  | RTARespond
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

instance FromText RespondToAfd where
  parser =
    takeLowerText >>= \case
      "none" -> pure RTANone
      "passthrough" -> pure RTAPassthrough
      "respond" -> pure RTARespond
      e ->
        fromTextError $
          "Failure parsing RespondToAfd from value: '" <> e
            <> "'. Accepted values: none, passthrough, respond"

instance ToText RespondToAfd where
  toText = \case
    RTANone -> "NONE"
    RTAPassthrough -> "PASSTHROUGH"
    RTARespond -> "RESPOND"

instance Hashable RespondToAfd

instance NFData RespondToAfd

instance ToByteString RespondToAfd

instance ToQuery RespondToAfd

instance ToHeader RespondToAfd

instance ToJSON RespondToAfd where
  toJSON = toJSONText

instance FromJSON RespondToAfd where
  parseJSON = parseJSONText "RespondToAfd"
