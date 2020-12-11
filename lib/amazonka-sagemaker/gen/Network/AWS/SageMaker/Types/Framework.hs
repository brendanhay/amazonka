-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Framework
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Framework
  ( Framework
      ( Framework',
        Darknet,
        Keras,
        Mxnet,
        Onnx,
        Pytorch,
        Tensorflow,
        Tflite,
        Xgboost
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Framework = Framework' Lude.Text
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

pattern Darknet :: Framework
pattern Darknet = Framework' "DARKNET"

pattern Keras :: Framework
pattern Keras = Framework' "KERAS"

pattern Mxnet :: Framework
pattern Mxnet = Framework' "MXNET"

pattern Onnx :: Framework
pattern Onnx = Framework' "ONNX"

pattern Pytorch :: Framework
pattern Pytorch = Framework' "PYTORCH"

pattern Tensorflow :: Framework
pattern Tensorflow = Framework' "TENSORFLOW"

pattern Tflite :: Framework
pattern Tflite = Framework' "TFLITE"

pattern Xgboost :: Framework
pattern Xgboost = Framework' "XGBOOST"

{-# COMPLETE
  Darknet,
  Keras,
  Mxnet,
  Onnx,
  Pytorch,
  Tensorflow,
  Tflite,
  Xgboost,
  Framework'
  #-}
