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
-- Module      : Network.AWS.SageMaker.Types.ParameterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParameterType
  ( ParameterType
      ( ..,
        ParameterType_Categorical,
        ParameterType_Continuous,
        ParameterType_FreeText,
        ParameterType_Integer
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ParameterType = ParameterType'
  { fromParameterType ::
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

pattern ParameterType_Categorical :: ParameterType
pattern ParameterType_Categorical = ParameterType' "Categorical"

pattern ParameterType_Continuous :: ParameterType
pattern ParameterType_Continuous = ParameterType' "Continuous"

pattern ParameterType_FreeText :: ParameterType
pattern ParameterType_FreeText = ParameterType' "FreeText"

pattern ParameterType_Integer :: ParameterType
pattern ParameterType_Integer = ParameterType' "Integer"

{-# COMPLETE
  ParameterType_Categorical,
  ParameterType_Continuous,
  ParameterType_FreeText,
  ParameterType_Integer,
  ParameterType'
  #-}
