{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DatePartitionDelimiterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.DatePartitionDelimiterValue
  ( DatePartitionDelimiterValue
    ( DatePartitionDelimiterValue'
    , DatePartitionDelimiterValueSlash
    , DatePartitionDelimiterValueUnderscore
    , DatePartitionDelimiterValueDash
    , DatePartitionDelimiterValueNone
    , fromDatePartitionDelimiterValue
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DatePartitionDelimiterValue = DatePartitionDelimiterValue'{fromDatePartitionDelimiterValue
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern DatePartitionDelimiterValueSlash :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValueSlash = DatePartitionDelimiterValue' "SLASH"

pattern DatePartitionDelimiterValueUnderscore :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValueUnderscore = DatePartitionDelimiterValue' "UNDERSCORE"

pattern DatePartitionDelimiterValueDash :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValueDash = DatePartitionDelimiterValue' "DASH"

pattern DatePartitionDelimiterValueNone :: DatePartitionDelimiterValue
pattern DatePartitionDelimiterValueNone = DatePartitionDelimiterValue' "NONE"

{-# COMPLETE 
  DatePartitionDelimiterValueSlash,

  DatePartitionDelimiterValueUnderscore,

  DatePartitionDelimiterValueDash,

  DatePartitionDelimiterValueNone,
  DatePartitionDelimiterValue'
  #-}
