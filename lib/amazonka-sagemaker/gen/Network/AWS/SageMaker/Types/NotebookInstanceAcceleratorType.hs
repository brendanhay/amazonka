{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType where

import Network.AWS.Prelude

data NotebookInstanceAcceleratorType
  = Ml_EIA1_Large
  | Ml_EIA1_Medium
  | Ml_EIA1_XLarge
  | Ml_EIA2_Large
  | Ml_EIA2_Medium
  | Ml_EIA2_XLarge
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

instance FromText NotebookInstanceAcceleratorType where
  parser =
    takeLowerText >>= \case
      "ml.eia1.large" -> pure Ml_EIA1_Large
      "ml.eia1.medium" -> pure Ml_EIA1_Medium
      "ml.eia1.xlarge" -> pure Ml_EIA1_XLarge
      "ml.eia2.large" -> pure Ml_EIA2_Large
      "ml.eia2.medium" -> pure Ml_EIA2_Medium
      "ml.eia2.xlarge" -> pure Ml_EIA2_XLarge
      e ->
        fromTextError $
          "Failure parsing NotebookInstanceAcceleratorType from value: '" <> e
            <> "'. Accepted values: ml.eia1.large, ml.eia1.medium, ml.eia1.xlarge, ml.eia2.large, ml.eia2.medium, ml.eia2.xlarge"

instance ToText NotebookInstanceAcceleratorType where
  toText = \case
    Ml_EIA1_Large -> "ml.eia1.large"
    Ml_EIA1_Medium -> "ml.eia1.medium"
    Ml_EIA1_XLarge -> "ml.eia1.xlarge"
    Ml_EIA2_Large -> "ml.eia2.large"
    Ml_EIA2_Medium -> "ml.eia2.medium"
    Ml_EIA2_XLarge -> "ml.eia2.xlarge"

instance Hashable NotebookInstanceAcceleratorType

instance NFData NotebookInstanceAcceleratorType

instance ToByteString NotebookInstanceAcceleratorType

instance ToQuery NotebookInstanceAcceleratorType

instance ToHeader NotebookInstanceAcceleratorType

instance ToJSON NotebookInstanceAcceleratorType where
  toJSON = toJSONText

instance FromJSON NotebookInstanceAcceleratorType where
  parseJSON = parseJSONText "NotebookInstanceAcceleratorType"
