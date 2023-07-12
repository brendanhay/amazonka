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
-- Module      : Amazonka.IoT.Types.CustomMetricType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CustomMetricType
  ( CustomMetricType
      ( ..,
        CustomMetricType_Ip_address_list,
        CustomMetricType_Number,
        CustomMetricType_Number_list,
        CustomMetricType_String_list
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomMetricType = CustomMetricType'
  { fromCustomMetricType ::
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
