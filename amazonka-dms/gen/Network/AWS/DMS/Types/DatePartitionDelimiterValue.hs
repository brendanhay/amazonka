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

import qualified Network.AWS.Prelude as Prelude

newtype DatePartitionDelimiterValue = DatePartitionDelimiterValue'
  { fromDatePartitionDelimiterValue ::
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
