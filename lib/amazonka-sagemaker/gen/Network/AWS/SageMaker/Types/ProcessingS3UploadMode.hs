{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3UploadMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3UploadMode where

import Network.AWS.Prelude

data ProcessingS3UploadMode
  = Continuous
  | EndOfJob
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

instance FromText ProcessingS3UploadMode where
  parser =
    takeLowerText >>= \case
      "continuous" -> pure Continuous
      "endofjob" -> pure EndOfJob
      e ->
        fromTextError $
          "Failure parsing ProcessingS3UploadMode from value: '" <> e
            <> "'. Accepted values: continuous, endofjob"

instance ToText ProcessingS3UploadMode where
  toText = \case
    Continuous -> "Continuous"
    EndOfJob -> "EndOfJob"

instance Hashable ProcessingS3UploadMode

instance NFData ProcessingS3UploadMode

instance ToByteString ProcessingS3UploadMode

instance ToQuery ProcessingS3UploadMode

instance ToHeader ProcessingS3UploadMode

instance ToJSON ProcessingS3UploadMode where
  toJSON = toJSONText

instance FromJSON ProcessingS3UploadMode where
  parseJSON = parseJSONText "ProcessingS3UploadMode"
