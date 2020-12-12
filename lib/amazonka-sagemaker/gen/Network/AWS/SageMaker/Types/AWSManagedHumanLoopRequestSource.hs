{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource
  ( AWSManagedHumanLoopRequestSource
      ( AWSManagedHumanLoopRequestSource',
        AWSRekognitionDetectModerationLabelsImageV3,
        AWSTextractAnalyzeDocumentFormsV1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AWSManagedHumanLoopRequestSource = AWSManagedHumanLoopRequestSource' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AWSRekognitionDetectModerationLabelsImageV3 :: AWSManagedHumanLoopRequestSource
pattern AWSRekognitionDetectModerationLabelsImageV3 = AWSManagedHumanLoopRequestSource' "AWS/Rekognition/DetectModerationLabels/Image/V3"

pattern AWSTextractAnalyzeDocumentFormsV1 :: AWSManagedHumanLoopRequestSource
pattern AWSTextractAnalyzeDocumentFormsV1 = AWSManagedHumanLoopRequestSource' "AWS/Textract/AnalyzeDocument/Forms/V1"

{-# COMPLETE
  AWSRekognitionDetectModerationLabelsImageV3,
  AWSTextractAnalyzeDocumentFormsV1,
  AWSManagedHumanLoopRequestSource'
  #-}
