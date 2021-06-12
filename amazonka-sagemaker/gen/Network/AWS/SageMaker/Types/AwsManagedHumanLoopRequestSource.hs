{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AwsManagedHumanLoopRequestSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AwsManagedHumanLoopRequestSource
  ( AwsManagedHumanLoopRequestSource
      ( ..,
        AwsManagedHumanLoopRequestSource_AWS_Rekognition_DetectModerationLabels_Image_V3,
        AwsManagedHumanLoopRequestSource_AWS_Textract_AnalyzeDocument_Forms_V1
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AwsManagedHumanLoopRequestSource = AwsManagedHumanLoopRequestSource'
  { fromAwsManagedHumanLoopRequestSource ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AwsManagedHumanLoopRequestSource_AWS_Rekognition_DetectModerationLabels_Image_V3 :: AwsManagedHumanLoopRequestSource
pattern AwsManagedHumanLoopRequestSource_AWS_Rekognition_DetectModerationLabels_Image_V3 = AwsManagedHumanLoopRequestSource' "AWS/Rekognition/DetectModerationLabels/Image/V3"

pattern AwsManagedHumanLoopRequestSource_AWS_Textract_AnalyzeDocument_Forms_V1 :: AwsManagedHumanLoopRequestSource
pattern AwsManagedHumanLoopRequestSource_AWS_Textract_AnalyzeDocument_Forms_V1 = AwsManagedHumanLoopRequestSource' "AWS/Textract/AnalyzeDocument/Forms/V1"

{-# COMPLETE
  AwsManagedHumanLoopRequestSource_AWS_Rekognition_DetectModerationLabels_Image_V3,
  AwsManagedHumanLoopRequestSource_AWS_Textract_AnalyzeDocument_Forms_V1,
  AwsManagedHumanLoopRequestSource'
  #-}
