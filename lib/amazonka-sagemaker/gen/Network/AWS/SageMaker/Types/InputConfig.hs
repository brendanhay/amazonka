{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Framework

-- | Contains information about the location of input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
--
--
-- /See:/ 'inputConfig' smart constructor.
data InputConfig = InputConfig'
  { _icS3URI :: !Text,
    _icDataInputConfig :: !Text,
    _icFramework :: !Framework
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icS3URI' - The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).
--
-- * 'icDataInputConfig' - Specifies the name and shape of the expected data inputs for your trained model with a JSON dictionary form. The data inputs are 'InputConfig$Framework' specific.      * @TensorFlow@ : You must specify the name and shape (NHWC format) of the expected data inputs using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"input":[1,1024,1024,3]}@      * If using the CLI, @{\"input\":[1,1024,1024,3]}@      * Examples for two inputs:     * If using the console, @{"data1": [1,28,28,1], "data2":[1,28,28,1]}@      * If using the CLI, @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@      * @KERAS@ : You must specify the name and shape (NCHW format) of expected data inputs using a dictionary format for your trained model. Note that while Keras model artifacts should be uploaded in NHWC (channel-last) format, @DataInputConfig@ should be specified in NCHW (channel-first) format. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"input_1":[1,3,224,224]}@      * If using the CLI, @{\"input_1\":[1,3,224,224]}@      * Examples for two inputs:     * If using the console, @{"input_1": [1,3,224,224], "input_2":[1,3,224,224]} @      * If using the CLI, @{\"input_1\": [1,3,224,224], \"input_2\":[1,3,224,224]}@      * @MXNET/ONNX/DARKNET@ : You must specify the name and shape (NCHW format) of the expected data inputs in order using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"data":[1,3,1024,1024]}@      * If using the CLI, @{\"data\":[1,3,1024,1024]}@      * Examples for two inputs:     * If using the console, @{"var1": [1,1,28,28], "var2":[1,1,28,28]} @      * If using the CLI, @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]}@      * @PyTorch@ : You can either specify the name and shape (NCHW format) of expected data inputs in order using a dictionary format for your trained model or you can specify the shape only using a list format. The dictionary formats required for the console and CLI are different. The list formats for the console and CLI are the same.     * Examples for one input in dictionary format:     * If using the console, @{"input0":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224]}@      * Example for one input in list format: @[[1,3,224,224]]@      * Examples for two inputs in dictionary format:     * If using the console, @{"input0":[1,3,224,224], "input1":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]} @      * Example for two inputs in list format: @[[1,3,224,224], [1,3,224,224]]@      * @XGBOOST@ : input data name and shape are not needed. @DataInputConfig@ supports the following parameters for @CoreML@ 'OutputConfig$TargetDevice' (ML Model format):     * @shape@ : Input shape, for example @{"input_1": {"shape": [1,224,224,3]}}@ . In addition to static input shapes, CoreML converter supports Flexible input shapes:     * Range Dimension. You can use the Range Dimension feature if you know the input shape will be within some specific interval in that dimension, for example: @{"input_1": {"shape": ["1..10", 224, 224, 3]}}@      * Enumerated shapes. Sometimes, the models are trained to work only on a select set of inputs. You can enumerate all supported input shapes, for example: @{"input_1": {"shape": [[1, 224, 224, 3], [1, 160, 160, 3]]}}@      * @default_shape@ : Default input shape. You can set a default shape during conversion for both Range Dimension and Enumerated Shapes. For example @{"input_1": {"shape": ["1..10", 224, 224, 3], "default_shape": [1, 224, 224, 3]}}@      * @type@ : Input type. Allowed values: @Image@ and @Tensor@ . By default, the converter generates an ML Model with inputs of type Tensor (MultiArray). User can set input type to be Image. Image input type requires additional input parameters such as @bias@ and @scale@ .     * @bias@ : If the input type is an Image, you need to provide the bias vector.     * @scale@ : If the input type is an Image, you need to provide a scale factor. CoreML @ClassifierConfig@ parameters can be specified using 'OutputConfig$CompilerOptions' . CoreML converter supports Tensorflow and PyTorch models. CoreML conversion examples:     * Tensor type input:     * @"DataInputConfig": {"input_1": {"shape": [[1,224,224,3], [1,160,160,3]], "default_shape": [1,224,224,3]}}@      * Tensor type input without input name (PyTorch):     * @"DataInputConfig": [{"shape": [[1,3,224,224], [1,3,160,160]], "default_shape": [1,3,224,224]}]@      * Image type input:     * @"DataInputConfig": {"input_1": {"shape": [[1,224,224,3], [1,160,160,3]], "default_shape": [1,224,224,3], "type": "Image", "bias": [-1,-1,-1], "scale": 0.007843137255}}@      * @"CompilerOptions": {"class_labels": "imagenet_labels_1000.txt"}@      * Image type input without input name (PyTorch):     * @"DataInputConfig": [{"shape": [[1,3,224,224], [1,3,160,160]], "default_shape": [1,3,224,224], "type": "Image", "bias": [-1,-1,-1], "scale": 0.007843137255}]@      * @"CompilerOptions": {"class_labels": "imagenet_labels_1000.txt"}@
--
-- * 'icFramework' - Identifies the framework in which the model was trained. For example: TENSORFLOW.
inputConfig ::
  -- | 'icS3URI'
  Text ->
  -- | 'icDataInputConfig'
  Text ->
  -- | 'icFramework'
  Framework ->
  InputConfig
inputConfig pS3URI_ pDataInputConfig_ pFramework_ =
  InputConfig'
    { _icS3URI = pS3URI_,
      _icDataInputConfig = pDataInputConfig_,
      _icFramework = pFramework_
    }

-- | The S3 path where the model artifacts, which result from model training, are stored. This path must point to a single gzip compressed tar archive (.tar.gz suffix).
icS3URI :: Lens' InputConfig Text
icS3URI = lens _icS3URI (\s a -> s {_icS3URI = a})

-- | Specifies the name and shape of the expected data inputs for your trained model with a JSON dictionary form. The data inputs are 'InputConfig$Framework' specific.      * @TensorFlow@ : You must specify the name and shape (NHWC format) of the expected data inputs using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"input":[1,1024,1024,3]}@      * If using the CLI, @{\"input\":[1,1024,1024,3]}@      * Examples for two inputs:     * If using the console, @{"data1": [1,28,28,1], "data2":[1,28,28,1]}@      * If using the CLI, @{\"data1\": [1,28,28,1], \"data2\":[1,28,28,1]}@      * @KERAS@ : You must specify the name and shape (NCHW format) of expected data inputs using a dictionary format for your trained model. Note that while Keras model artifacts should be uploaded in NHWC (channel-last) format, @DataInputConfig@ should be specified in NCHW (channel-first) format. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"input_1":[1,3,224,224]}@      * If using the CLI, @{\"input_1\":[1,3,224,224]}@      * Examples for two inputs:     * If using the console, @{"input_1": [1,3,224,224], "input_2":[1,3,224,224]} @      * If using the CLI, @{\"input_1\": [1,3,224,224], \"input_2\":[1,3,224,224]}@      * @MXNET/ONNX/DARKNET@ : You must specify the name and shape (NCHW format) of the expected data inputs in order using a dictionary format for your trained model. The dictionary formats required for the console and CLI are different.     * Examples for one input:     * If using the console, @{"data":[1,3,1024,1024]}@      * If using the CLI, @{\"data\":[1,3,1024,1024]}@      * Examples for two inputs:     * If using the console, @{"var1": [1,1,28,28], "var2":[1,1,28,28]} @      * If using the CLI, @{\"var1\": [1,1,28,28], \"var2\":[1,1,28,28]}@      * @PyTorch@ : You can either specify the name and shape (NCHW format) of expected data inputs in order using a dictionary format for your trained model or you can specify the shape only using a list format. The dictionary formats required for the console and CLI are different. The list formats for the console and CLI are the same.     * Examples for one input in dictionary format:     * If using the console, @{"input0":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224]}@      * Example for one input in list format: @[[1,3,224,224]]@      * Examples for two inputs in dictionary format:     * If using the console, @{"input0":[1,3,224,224], "input1":[1,3,224,224]}@      * If using the CLI, @{\"input0\":[1,3,224,224], \"input1\":[1,3,224,224]} @      * Example for two inputs in list format: @[[1,3,224,224], [1,3,224,224]]@      * @XGBOOST@ : input data name and shape are not needed. @DataInputConfig@ supports the following parameters for @CoreML@ 'OutputConfig$TargetDevice' (ML Model format):     * @shape@ : Input shape, for example @{"input_1": {"shape": [1,224,224,3]}}@ . In addition to static input shapes, CoreML converter supports Flexible input shapes:     * Range Dimension. You can use the Range Dimension feature if you know the input shape will be within some specific interval in that dimension, for example: @{"input_1": {"shape": ["1..10", 224, 224, 3]}}@      * Enumerated shapes. Sometimes, the models are trained to work only on a select set of inputs. You can enumerate all supported input shapes, for example: @{"input_1": {"shape": [[1, 224, 224, 3], [1, 160, 160, 3]]}}@      * @default_shape@ : Default input shape. You can set a default shape during conversion for both Range Dimension and Enumerated Shapes. For example @{"input_1": {"shape": ["1..10", 224, 224, 3], "default_shape": [1, 224, 224, 3]}}@      * @type@ : Input type. Allowed values: @Image@ and @Tensor@ . By default, the converter generates an ML Model with inputs of type Tensor (MultiArray). User can set input type to be Image. Image input type requires additional input parameters such as @bias@ and @scale@ .     * @bias@ : If the input type is an Image, you need to provide the bias vector.     * @scale@ : If the input type is an Image, you need to provide a scale factor. CoreML @ClassifierConfig@ parameters can be specified using 'OutputConfig$CompilerOptions' . CoreML converter supports Tensorflow and PyTorch models. CoreML conversion examples:     * Tensor type input:     * @"DataInputConfig": {"input_1": {"shape": [[1,224,224,3], [1,160,160,3]], "default_shape": [1,224,224,3]}}@      * Tensor type input without input name (PyTorch):     * @"DataInputConfig": [{"shape": [[1,3,224,224], [1,3,160,160]], "default_shape": [1,3,224,224]}]@      * Image type input:     * @"DataInputConfig": {"input_1": {"shape": [[1,224,224,3], [1,160,160,3]], "default_shape": [1,224,224,3], "type": "Image", "bias": [-1,-1,-1], "scale": 0.007843137255}}@      * @"CompilerOptions": {"class_labels": "imagenet_labels_1000.txt"}@      * Image type input without input name (PyTorch):     * @"DataInputConfig": [{"shape": [[1,3,224,224], [1,3,160,160]], "default_shape": [1,3,224,224], "type": "Image", "bias": [-1,-1,-1], "scale": 0.007843137255}]@      * @"CompilerOptions": {"class_labels": "imagenet_labels_1000.txt"}@
icDataInputConfig :: Lens' InputConfig Text
icDataInputConfig = lens _icDataInputConfig (\s a -> s {_icDataInputConfig = a})

-- | Identifies the framework in which the model was trained. For example: TENSORFLOW.
icFramework :: Lens' InputConfig Framework
icFramework = lens _icFramework (\s a -> s {_icFramework = a})

instance FromJSON InputConfig where
  parseJSON =
    withObject
      "InputConfig"
      ( \x ->
          InputConfig'
            <$> (x .: "S3Uri") <*> (x .: "DataInputConfig") <*> (x .: "Framework")
      )

instance Hashable InputConfig

instance NFData InputConfig

instance ToJSON InputConfig where
  toJSON InputConfig' {..} =
    object
      ( catMaybes
          [ Just ("S3Uri" .= _icS3URI),
            Just ("DataInputConfig" .= _icDataInputConfig),
            Just ("Framework" .= _icFramework)
          ]
      )
