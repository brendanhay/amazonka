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

import qualified Network.AWS.Prelude as Prelude

newtype CustomMetricType = CustomMetricType'
  { fromCustomMetricType ::
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
