{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModelType
  ( MLModelType
      ( MLModelType',
        MLModelTypeRegression,
        MLModelTypeBinary,
        MLModelTypeMulticlass,
        fromMLModelType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MLModelType = MLModelType' {fromMLModelType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern MLModelTypeRegression :: MLModelType
pattern MLModelTypeRegression = MLModelType' "REGRESSION"

pattern MLModelTypeBinary :: MLModelType
pattern MLModelTypeBinary = MLModelType' "BINARY"

pattern MLModelTypeMulticlass :: MLModelType
pattern MLModelTypeMulticlass = MLModelType' "MULTICLASS"

{-# COMPLETE
  MLModelTypeRegression,
  MLModelTypeBinary,
  MLModelTypeMulticlass,
  MLModelType'
  #-}
