{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource where

import Network.AWS.Prelude

data AWSManagedHumanLoopRequestSource
  = AWSRekognitionDetectModerationLabelsImageV3
  | AWSTextractAnalyzeDocumentFormsV1
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

instance FromText AWSManagedHumanLoopRequestSource where
  parser =
    takeLowerText >>= \case
      "aws/rekognition/detectmoderationlabels/image/v3" -> pure AWSRekognitionDetectModerationLabelsImageV3
      "aws/textract/analyzedocument/forms/v1" -> pure AWSTextractAnalyzeDocumentFormsV1
      e ->
        fromTextError $
          "Failure parsing AWSManagedHumanLoopRequestSource from value: '" <> e
            <> "'. Accepted values: aws/rekognition/detectmoderationlabels/image/v3, aws/textract/analyzedocument/forms/v1"

instance ToText AWSManagedHumanLoopRequestSource where
  toText = \case
    AWSRekognitionDetectModerationLabelsImageV3 -> "AWS/Rekognition/DetectModerationLabels/Image/V3"
    AWSTextractAnalyzeDocumentFormsV1 -> "AWS/Textract/AnalyzeDocument/Forms/V1"

instance Hashable AWSManagedHumanLoopRequestSource

instance NFData AWSManagedHumanLoopRequestSource

instance ToByteString AWSManagedHumanLoopRequestSource

instance ToQuery AWSManagedHumanLoopRequestSource

instance ToHeader AWSManagedHumanLoopRequestSource

instance ToJSON AWSManagedHumanLoopRequestSource where
  toJSON = toJSONText

instance FromJSON AWSManagedHumanLoopRequestSource where
  parseJSON = parseJSONText "AWSManagedHumanLoopRequestSource"
