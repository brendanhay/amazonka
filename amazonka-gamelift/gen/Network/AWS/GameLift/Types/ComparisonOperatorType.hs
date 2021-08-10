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
-- Module      : Network.AWS.GameLift.Types.ComparisonOperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ComparisonOperatorType
  ( ComparisonOperatorType
      ( ..,
        ComparisonOperatorType_GreaterThanOrEqualToThreshold,
        ComparisonOperatorType_GreaterThanThreshold,
        ComparisonOperatorType_LessThanOrEqualToThreshold,
        ComparisonOperatorType_LessThanThreshold
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ComparisonOperatorType = ComparisonOperatorType'
  { fromComparisonOperatorType ::
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

pattern ComparisonOperatorType_GreaterThanOrEqualToThreshold :: ComparisonOperatorType
pattern ComparisonOperatorType_GreaterThanOrEqualToThreshold = ComparisonOperatorType' "GreaterThanOrEqualToThreshold"

pattern ComparisonOperatorType_GreaterThanThreshold :: ComparisonOperatorType
pattern ComparisonOperatorType_GreaterThanThreshold = ComparisonOperatorType' "GreaterThanThreshold"

pattern ComparisonOperatorType_LessThanOrEqualToThreshold :: ComparisonOperatorType
pattern ComparisonOperatorType_LessThanOrEqualToThreshold = ComparisonOperatorType' "LessThanOrEqualToThreshold"

pattern ComparisonOperatorType_LessThanThreshold :: ComparisonOperatorType
pattern ComparisonOperatorType_LessThanThreshold = ComparisonOperatorType' "LessThanThreshold"

{-# COMPLETE
  ComparisonOperatorType_GreaterThanOrEqualToThreshold,
  ComparisonOperatorType_GreaterThanThreshold,
  ComparisonOperatorType_LessThanOrEqualToThreshold,
  ComparisonOperatorType_LessThanThreshold,
  ComparisonOperatorType'
  #-}
