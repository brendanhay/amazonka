{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProblemType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProblemType where

import Network.AWS.Prelude

data ProblemType
  = BinaryClassification
  | MulticlassClassification
  | Regression
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

instance FromText ProblemType where
  parser =
    takeLowerText >>= \case
      "binaryclassification" -> pure BinaryClassification
      "multiclassclassification" -> pure MulticlassClassification
      "regression" -> pure Regression
      e ->
        fromTextError $
          "Failure parsing ProblemType from value: '" <> e
            <> "'. Accepted values: binaryclassification, multiclassclassification, regression"

instance ToText ProblemType where
  toText = \case
    BinaryClassification -> "BinaryClassification"
    MulticlassClassification -> "MulticlassClassification"
    Regression -> "Regression"

instance Hashable ProblemType

instance NFData ProblemType

instance ToByteString ProblemType

instance ToQuery ProblemType

instance ToHeader ProblemType

instance ToJSON ProblemType where
  toJSON = toJSONText

instance FromJSON ProblemType where
  parseJSON = parseJSONText "ProblemType"
