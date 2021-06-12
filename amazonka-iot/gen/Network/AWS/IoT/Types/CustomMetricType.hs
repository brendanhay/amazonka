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
-- Module      : Network.AWS.IoT.Types.CustomMetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CustomMetricType
  ( CustomMetricType
      ( ..,
        CustomMetricType_Ip_address_list,
        CustomMetricType_Number,
        CustomMetricType_Number_list,
        CustomMetricType_String_list
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CustomMetricType = CustomMetricType'
  { fromCustomMetricType ::
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

pattern CustomMetricType_Ip_address_list :: CustomMetricType
pattern CustomMetricType_Ip_address_list = CustomMetricType' "ip-address-list"

pattern CustomMetricType_Number :: CustomMetricType
pattern CustomMetricType_Number = CustomMetricType' "number"

pattern CustomMetricType_Number_list :: CustomMetricType
pattern CustomMetricType_Number_list = CustomMetricType' "number-list"

pattern CustomMetricType_String_list :: CustomMetricType
pattern CustomMetricType_String_list = CustomMetricType' "string-list"

{-# COMPLETE
  CustomMetricType_Ip_address_list,
  CustomMetricType_Number,
  CustomMetricType_Number_list,
  CustomMetricType_String_list,
  CustomMetricType'
  #-}
