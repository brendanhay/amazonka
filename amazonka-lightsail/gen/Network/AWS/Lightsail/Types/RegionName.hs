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
-- Module      : Network.AWS.Lightsail.Types.RegionName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionName
  ( RegionName
      ( ..,
        RegionName_Ap_northeast_1,
        RegionName_Ap_northeast_2,
        RegionName_Ap_south_1,
        RegionName_Ap_southeast_1,
        RegionName_Ap_southeast_2,
        RegionName_Ca_central_1,
        RegionName_Eu_central_1,
        RegionName_Eu_west_1,
        RegionName_Eu_west_2,
        RegionName_Eu_west_3,
        RegionName_Us_east_1,
        RegionName_Us_east_2,
        RegionName_Us_west_1,
        RegionName_Us_west_2
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RegionName = RegionName'
  { fromRegionName ::
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

pattern RegionName_Ap_northeast_1 :: RegionName
pattern RegionName_Ap_northeast_1 = RegionName' "ap-northeast-1"

pattern RegionName_Ap_northeast_2 :: RegionName
pattern RegionName_Ap_northeast_2 = RegionName' "ap-northeast-2"

pattern RegionName_Ap_south_1 :: RegionName
pattern RegionName_Ap_south_1 = RegionName' "ap-south-1"

pattern RegionName_Ap_southeast_1 :: RegionName
pattern RegionName_Ap_southeast_1 = RegionName' "ap-southeast-1"

pattern RegionName_Ap_southeast_2 :: RegionName
pattern RegionName_Ap_southeast_2 = RegionName' "ap-southeast-2"

pattern RegionName_Ca_central_1 :: RegionName
pattern RegionName_Ca_central_1 = RegionName' "ca-central-1"

pattern RegionName_Eu_central_1 :: RegionName
pattern RegionName_Eu_central_1 = RegionName' "eu-central-1"

pattern RegionName_Eu_west_1 :: RegionName
pattern RegionName_Eu_west_1 = RegionName' "eu-west-1"

pattern RegionName_Eu_west_2 :: RegionName
pattern RegionName_Eu_west_2 = RegionName' "eu-west-2"

pattern RegionName_Eu_west_3 :: RegionName
pattern RegionName_Eu_west_3 = RegionName' "eu-west-3"

pattern RegionName_Us_east_1 :: RegionName
pattern RegionName_Us_east_1 = RegionName' "us-east-1"

pattern RegionName_Us_east_2 :: RegionName
pattern RegionName_Us_east_2 = RegionName' "us-east-2"

pattern RegionName_Us_west_1 :: RegionName
pattern RegionName_Us_west_1 = RegionName' "us-west-1"

pattern RegionName_Us_west_2 :: RegionName
pattern RegionName_Us_west_2 = RegionName' "us-west-2"

{-# COMPLETE
  RegionName_Ap_northeast_1,
  RegionName_Ap_northeast_2,
  RegionName_Ap_south_1,
  RegionName_Ap_southeast_1,
  RegionName_Ap_southeast_2,
  RegionName_Ca_central_1,
  RegionName_Eu_central_1,
  RegionName_Eu_west_1,
  RegionName_Eu_west_2,
  RegionName_Eu_west_3,
  RegionName_Us_east_1,
  RegionName_Us_east_2,
  RegionName_Us_west_1,
  RegionName_Us_west_2,
  RegionName'
  #-}
