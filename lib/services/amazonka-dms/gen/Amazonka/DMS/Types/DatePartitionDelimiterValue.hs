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
-- Module      : Amazonka.DMS.Types.DatePartitionDelimiterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DatePartitionDelimiterValue
  ( DatePartitionDelimiterValue
      ( ..,
        DatePartitionDelimiterValue_DASH,
        DatePartitionDelimiterValue_NONE,
        DatePartitionDelimiterValue_SLASH,
        DatePartitionDelimiterValue_UNDERSCORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatePartitionDelimiterValue = DatePartitionDelimiterValue'
  { fromDatePartitionDelimiterValue ::
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
