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
-- Module      : Network.AWS.WAFRegional.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_EQ,
        ComparisonOperator_GE,
        ComparisonOperator_GT,
        ComparisonOperator_LE,
        ComparisonOperator_LT,
        ComparisonOperator_NE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
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

pattern ComparisonOperator_EQ :: ComparisonOperator
pattern ComparisonOperator_EQ = ComparisonOperator' "EQ"

pattern ComparisonOperator_GE :: ComparisonOperator
pattern ComparisonOperator_GE = ComparisonOperator' "GE"

pattern ComparisonOperator_GT :: ComparisonOperator
pattern ComparisonOperator_GT = ComparisonOperator' "GT"

pattern ComparisonOperator_LE :: ComparisonOperator
pattern ComparisonOperator_LE = ComparisonOperator' "LE"

pattern ComparisonOperator_LT :: ComparisonOperator
pattern ComparisonOperator_LT = ComparisonOperator' "LT"

pattern ComparisonOperator_NE :: ComparisonOperator
pattern ComparisonOperator_NE = ComparisonOperator' "NE"

{-# COMPLETE
  ComparisonOperator_EQ,
  ComparisonOperator_GE,
  ComparisonOperator_GT,
  ComparisonOperator_LE,
  ComparisonOperator_LT,
  ComparisonOperator_NE,
  ComparisonOperator'
  #-}
