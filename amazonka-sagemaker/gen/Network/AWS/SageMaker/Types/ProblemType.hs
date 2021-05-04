{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ProblemType = ProblemType'
  { fromProblemType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
