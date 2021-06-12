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
-- Module      : Network.AWS.SageMaker.Types.ProblemType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProblemType
  ( ProblemType
      ( ..,
        ProblemType_BinaryClassification,
        ProblemType_MulticlassClassification,
        ProblemType_Regression
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProblemType = ProblemType'
  { fromProblemType ::
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

pattern ProblemType_BinaryClassification :: ProblemType
pattern ProblemType_BinaryClassification = ProblemType' "BinaryClassification"

pattern ProblemType_MulticlassClassification :: ProblemType
pattern ProblemType_MulticlassClassification = ProblemType' "MulticlassClassification"

pattern ProblemType_Regression :: ProblemType
pattern ProblemType_Regression = ProblemType' "Regression"

{-# COMPLETE
  ProblemType_BinaryClassification,
  ProblemType_MulticlassClassification,
  ProblemType_Regression,
  ProblemType'
  #-}
