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
-- Module      : Amazonka.SageMaker.Types.AwsManagedHumanLoopRequestSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AwsManagedHumanLoopRequestSource
  ( AwsManagedHumanLoopRequestSource
      ( ..,
        AwsManagedHumanLoopRequestSource_AWS_Rekognition_DetectModerationLabels_Image_V3,
        AwsManagedHumanLoopRequestSource_AWS_Textract_AnalyzeDocument_Forms_V1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AwsManagedHumanLoopRequestSource = AwsManagedHumanLoopRequestSource'
  { fromAwsManagedHumanLoopRequestSource ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
