{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3InputMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3InputMode where

import Network.AWS.Prelude

data ProcessingS3InputMode
  = PSIMFile
  | PSIMPipe
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

instance FromText ProcessingS3InputMode where
  parser =
    takeLowerText >>= \case
      "file" -> pure PSIMFile
      "pipe" -> pure PSIMPipe
      e ->
        fromTextError $
          "Failure parsing ProcessingS3InputMode from value: '" <> e
            <> "'. Accepted values: file, pipe"

instance ToText ProcessingS3InputMode where
  toText = \case
    PSIMFile -> "File"
    PSIMPipe -> "Pipe"

instance Hashable ProcessingS3InputMode

instance NFData ProcessingS3InputMode

instance ToByteString ProcessingS3InputMode

instance ToQuery ProcessingS3InputMode

instance ToHeader ProcessingS3InputMode

instance ToJSON ProcessingS3InputMode where
  toJSON = toJSONText

instance FromJSON ProcessingS3InputMode where
  parseJSON = parseJSONText "ProcessingS3InputMode"
