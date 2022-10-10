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
-- Module      : Amazonka.SageMaker.Types.ParameterType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ParameterType
  ( ParameterType
      ( ..,
        ParameterType_Categorical,
        ParameterType_Continuous,
        ParameterType_FreeText,
        ParameterType_Integer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ParameterType = ParameterType'
  { fromParameterType ::
      Core.Text
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
