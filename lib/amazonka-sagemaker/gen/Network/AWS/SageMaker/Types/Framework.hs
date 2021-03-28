{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Framework
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Framework
  ( Framework
    ( Framework'
    , FrameworkTensorflow
    , FrameworkKeras
    , FrameworkMxnet
    , FrameworkOnnx
    , FrameworkPytorch
    , FrameworkXgboost
    , FrameworkTflite
    , FrameworkDarknet
    , fromFramework
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Framework = Framework'{fromFramework :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern FrameworkTensorflow :: Framework
pattern FrameworkTensorflow = Framework' "TENSORFLOW"

pattern FrameworkKeras :: Framework
pattern FrameworkKeras = Framework' "KERAS"

pattern FrameworkMxnet :: Framework
pattern FrameworkMxnet = Framework' "MXNET"

pattern FrameworkOnnx :: Framework
pattern FrameworkOnnx = Framework' "ONNX"

pattern FrameworkPytorch :: Framework
pattern FrameworkPytorch = Framework' "PYTORCH"

pattern FrameworkXgboost :: Framework
pattern FrameworkXgboost = Framework' "XGBOOST"

pattern FrameworkTflite :: Framework
pattern FrameworkTflite = Framework' "TFLITE"

pattern FrameworkDarknet :: Framework
pattern FrameworkDarknet = Framework' "DARKNET"

{-# COMPLETE 
  FrameworkTensorflow,

  FrameworkKeras,

  FrameworkMxnet,

  FrameworkOnnx,

  FrameworkPytorch,

  FrameworkXgboost,

  FrameworkTflite,

  FrameworkDarknet,
  Framework'
  #-}
