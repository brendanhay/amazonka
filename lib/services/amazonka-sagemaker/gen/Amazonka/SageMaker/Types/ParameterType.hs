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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParameterType = ParameterType'
  { fromParameterType ::
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
