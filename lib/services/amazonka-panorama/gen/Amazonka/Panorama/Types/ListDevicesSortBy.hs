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
-- Module      : Amazonka.Panorama.Types.ListDevicesSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ListDevicesSortBy
  ( ListDevicesSortBy
      ( ..,
        ListDevicesSortBy_CREATED_TIME,
        ListDevicesSortBy_DEVICE_AGGREGATED_STATUS,
        ListDevicesSortBy_DEVICE_ID,
        ListDevicesSortBy_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListDevicesSortBy = ListDevicesSortBy'
  { fromListDevicesSortBy ::
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

pattern ListDevicesSortBy_CREATED_TIME :: ListDevicesSortBy
pattern ListDevicesSortBy_CREATED_TIME = ListDevicesSortBy' "CREATED_TIME"

pattern ListDevicesSortBy_DEVICE_AGGREGATED_STATUS :: ListDevicesSortBy
pattern ListDevicesSortBy_DEVICE_AGGREGATED_STATUS = ListDevicesSortBy' "DEVICE_AGGREGATED_STATUS"

pattern ListDevicesSortBy_DEVICE_ID :: ListDevicesSortBy
pattern ListDevicesSortBy_DEVICE_ID = ListDevicesSortBy' "DEVICE_ID"

pattern ListDevicesSortBy_NAME :: ListDevicesSortBy
pattern ListDevicesSortBy_NAME = ListDevicesSortBy' "NAME"

{-# COMPLETE
  ListDevicesSortBy_CREATED_TIME,
  ListDevicesSortBy_DEVICE_AGGREGATED_STATUS,
  ListDevicesSortBy_DEVICE_ID,
  ListDevicesSortBy_NAME,
  ListDevicesSortBy'
  #-}
