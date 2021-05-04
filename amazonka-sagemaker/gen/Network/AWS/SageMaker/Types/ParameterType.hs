{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ParameterType = ParameterType'
  { fromParameterType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
