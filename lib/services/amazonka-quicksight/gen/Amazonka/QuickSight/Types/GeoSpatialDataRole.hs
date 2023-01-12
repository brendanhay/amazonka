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
-- Module      : Amazonka.QuickSight.Types.GeoSpatialDataRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeoSpatialDataRole
  ( GeoSpatialDataRole
      ( ..,
        GeoSpatialDataRole_CITY,
        GeoSpatialDataRole_COUNTRY,
        GeoSpatialDataRole_COUNTY,
        GeoSpatialDataRole_LATITUDE,
        GeoSpatialDataRole_LONGITUDE,
        GeoSpatialDataRole_POSTCODE,
        GeoSpatialDataRole_STATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GeoSpatialDataRole = GeoSpatialDataRole'
  { fromGeoSpatialDataRole ::
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

pattern GeoSpatialDataRole_CITY :: GeoSpatialDataRole
pattern GeoSpatialDataRole_CITY = GeoSpatialDataRole' "CITY"

pattern GeoSpatialDataRole_COUNTRY :: GeoSpatialDataRole
pattern GeoSpatialDataRole_COUNTRY = GeoSpatialDataRole' "COUNTRY"

pattern GeoSpatialDataRole_COUNTY :: GeoSpatialDataRole
pattern GeoSpatialDataRole_COUNTY = GeoSpatialDataRole' "COUNTY"

pattern GeoSpatialDataRole_LATITUDE :: GeoSpatialDataRole
pattern GeoSpatialDataRole_LATITUDE = GeoSpatialDataRole' "LATITUDE"

pattern GeoSpatialDataRole_LONGITUDE :: GeoSpatialDataRole
pattern GeoSpatialDataRole_LONGITUDE = GeoSpatialDataRole' "LONGITUDE"

pattern GeoSpatialDataRole_POSTCODE :: GeoSpatialDataRole
pattern GeoSpatialDataRole_POSTCODE = GeoSpatialDataRole' "POSTCODE"

pattern GeoSpatialDataRole_STATE :: GeoSpatialDataRole
pattern GeoSpatialDataRole_STATE = GeoSpatialDataRole' "STATE"

{-# COMPLETE
  GeoSpatialDataRole_CITY,
  GeoSpatialDataRole_COUNTRY,
  GeoSpatialDataRole_COUNTY,
  GeoSpatialDataRole_LATITUDE,
  GeoSpatialDataRole_LONGITUDE,
  GeoSpatialDataRole_POSTCODE,
  GeoSpatialDataRole_STATE,
  GeoSpatialDataRole'
  #-}
