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
-- Module      : Network.AWS.DMS.Types.DatePartitionDelimiterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DatePartitionDelimiterValue
  ( DatePartitionDelimiterValue
      ( ..,
        DatePartitionDelimiterValue_DASH,
        DatePartitionDelimiterValue_NONE,
        DatePartitionDelimiterValue_SLASH,
        DatePartitionDelimiterValue_UNDERSCORE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DatePartitionDelimiterValue = DatePartitionDelimiterValue'
  { fromDatePartitionDelimiterValue ::
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

pattern DatePartitionDelimiterValue_DASH :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValue_DASH = DatePartitionDelimiterValue' "DASH"

pattern DatePartitionDelimiterValue_NONE :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValue_NONE = DatePartitionDelimiterValue' "NONE"

pattern DatePartitionDelimiterValue_SLASH :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValue_SLASH = DatePartitionDelimiterValue' "SLASH"

pattern DatePartitionDelimiterValue_UNDERSCORE :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValue_UNDERSCORE = DatePartitionDelimiterValue' "UNDERSCORE"

{-# COMPLETE
  DatePartitionDelimiterValue_DASH,
  DatePartitionDelimiterValue_NONE,
  DatePartitionDelimiterValue_SLASH,
  DatePartitionDelimiterValue_UNDERSCORE,
  DatePartitionDelimiterValue'
  #-}
