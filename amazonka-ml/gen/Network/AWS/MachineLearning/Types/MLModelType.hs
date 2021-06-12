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
-- Module      : Network.AWS.MachineLearning.Types.MLModelType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModelType
  ( MLModelType
      ( ..,
        MLModelType_BINARY,
        MLModelType_MULTICLASS,
        MLModelType_REGRESSION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MLModelType = MLModelType'
  { fromMLModelType ::
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

pattern MLModelType_BINARY :: MLModelType
pattern MLModelType_BINARY = MLModelType' "BINARY"

pattern MLModelType_MULTICLASS :: MLModelType
pattern MLModelType_MULTICLASS = MLModelType' "MULTICLASS"

pattern MLModelType_REGRESSION :: MLModelType
pattern MLModelType_REGRESSION = MLModelType' "REGRESSION"

{-# COMPLETE
  MLModelType_BINARY,
  MLModelType_MULTICLASS,
  MLModelType_REGRESSION,
  MLModelType'
  #-}
