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
-- Module      : Amazonka.SageMaker.Types.Framework
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Framework
  ( Framework
      ( ..,
        Framework_DARKNET,
        Framework_KERAS,
        Framework_MXNET,
        Framework_ONNX,
        Framework_PYTORCH,
        Framework_SKLEARN,
        Framework_TENSORFLOW,
        Framework_TFLITE,
        Framework_XGBOOST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Framework = Framework'
  { fromFramework ::
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

pattern Framework_DARKNET :: Framework
pattern Framework_DARKNET = Framework' "DARKNET"

pattern Framework_KERAS :: Framework
pattern Framework_KERAS = Framework' "KERAS"

pattern Framework_MXNET :: Framework
pattern Framework_MXNET = Framework' "MXNET"

pattern Framework_ONNX :: Framework
pattern Framework_ONNX = Framework' "ONNX"

pattern Framework_PYTORCH :: Framework
pattern Framework_PYTORCH = Framework' "PYTORCH"

pattern Framework_SKLEARN :: Framework
pattern Framework_SKLEARN = Framework' "SKLEARN"

pattern Framework_TENSORFLOW :: Framework
pattern Framework_TENSORFLOW = Framework' "TENSORFLOW"

pattern Framework_TFLITE :: Framework
pattern Framework_TFLITE = Framework' "TFLITE"

pattern Framework_XGBOOST :: Framework
pattern Framework_XGBOOST = Framework' "XGBOOST"

{-# COMPLETE
  Framework_DARKNET,
  Framework_KERAS,
  Framework_MXNET,
  Framework_ONNX,
  Framework_PYTORCH,
  Framework_SKLEARN,
  Framework_TENSORFLOW,
  Framework_TFLITE,
  Framework_XGBOOST,
  Framework'
  #-}
