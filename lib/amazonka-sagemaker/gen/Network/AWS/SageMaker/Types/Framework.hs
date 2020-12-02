{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Framework
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Framework where

import Network.AWS.Prelude

data Framework
  = Darknet
  | Keras
  | Mxnet
  | Onnx
  | Pytorch
  | Tensorflow
  | Tflite
  | Xgboost
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

instance FromText Framework where
  parser =
    takeLowerText >>= \case
      "darknet" -> pure Darknet
      "keras" -> pure Keras
      "mxnet" -> pure Mxnet
      "onnx" -> pure Onnx
      "pytorch" -> pure Pytorch
      "tensorflow" -> pure Tensorflow
      "tflite" -> pure Tflite
      "xgboost" -> pure Xgboost
      e ->
        fromTextError $
          "Failure parsing Framework from value: '" <> e
            <> "'. Accepted values: darknet, keras, mxnet, onnx, pytorch, tensorflow, tflite, xgboost"

instance ToText Framework where
  toText = \case
    Darknet -> "DARKNET"
    Keras -> "KERAS"
    Mxnet -> "MXNET"
    Onnx -> "ONNX"
    Pytorch -> "PYTORCH"
    Tensorflow -> "TENSORFLOW"
    Tflite -> "TFLITE"
    Xgboost -> "XGBOOST"

instance Hashable Framework

instance NFData Framework

instance ToByteString Framework

instance ToQuery Framework

instance ToHeader Framework

instance ToJSON Framework where
  toJSON = toJSONText

instance FromJSON Framework where
  parseJSON = parseJSONText "Framework"
