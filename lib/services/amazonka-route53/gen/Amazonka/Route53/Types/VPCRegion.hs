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
-- Module      : Amazonka.Route53.Types.VPCRegion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.VPCRegion
  ( VPCRegion
      ( ..,
        VPCRegion_Af_south_1,
        VPCRegion_Ap_east_1,
        VPCRegion_Ap_northeast_1,
        VPCRegion_Ap_northeast_2,
        VPCRegion_Ap_northeast_3,
        VPCRegion_Ap_south_1,
        VPCRegion_Ap_south_2,
        VPCRegion_Ap_southeast_1,
        VPCRegion_Ap_southeast_2,
        VPCRegion_Ap_southeast_3,
        VPCRegion_Ca_central_1,
        VPCRegion_Cn_north_1,
        VPCRegion_Eu_central_1,
        VPCRegion_Eu_central_2,
        VPCRegion_Eu_north_1,
        VPCRegion_Eu_south_1,
        VPCRegion_Eu_south_2,
        VPCRegion_Eu_west_1,
        VPCRegion_Eu_west_2,
        VPCRegion_Eu_west_3,
        VPCRegion_Me_central_1,
        VPCRegion_Me_south_1,
        VPCRegion_Sa_east_1,
        VPCRegion_Us_east_1,
        VPCRegion_Us_east_2,
        VPCRegion_Us_gov_east_1,
        VPCRegion_Us_gov_west_1,
        VPCRegion_Us_iso_east_1,
        VPCRegion_Us_iso_west_1,
        VPCRegion_Us_isob_east_1,
        VPCRegion_Us_west_1,
        VPCRegion_Us_west_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype VPCRegion = VPCRegion'
  { fromVPCRegion ::
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

pattern VPCRegion_Af_south_1 :: VPCRegion
pattern VPCRegion_Af_south_1 = VPCRegion' "af-south-1"

pattern VPCRegion_Ap_east_1 :: VPCRegion
pattern VPCRegion_Ap_east_1 = VPCRegion' "ap-east-1"

pattern VPCRegion_Ap_northeast_1 :: VPCRegion
pattern VPCRegion_Ap_northeast_1 = VPCRegion' "ap-northeast-1"

pattern VPCRegion_Ap_northeast_2 :: VPCRegion
pattern VPCRegion_Ap_northeast_2 = VPCRegion' "ap-northeast-2"

pattern VPCRegion_Ap_northeast_3 :: VPCRegion
pattern VPCRegion_Ap_northeast_3 = VPCRegion' "ap-northeast-3"

pattern VPCRegion_Ap_south_1 :: VPCRegion
pattern VPCRegion_Ap_south_1 = VPCRegion' "ap-south-1"

pattern VPCRegion_Ap_south_2 :: VPCRegion
pattern VPCRegion_Ap_south_2 = VPCRegion' "ap-south-2"

pattern VPCRegion_Ap_southeast_1 :: VPCRegion
pattern VPCRegion_Ap_southeast_1 = VPCRegion' "ap-southeast-1"

pattern VPCRegion_Ap_southeast_2 :: VPCRegion
pattern VPCRegion_Ap_southeast_2 = VPCRegion' "ap-southeast-2"

pattern VPCRegion_Ap_southeast_3 :: VPCRegion
pattern VPCRegion_Ap_southeast_3 = VPCRegion' "ap-southeast-3"

pattern VPCRegion_Ca_central_1 :: VPCRegion
pattern VPCRegion_Ca_central_1 = VPCRegion' "ca-central-1"

pattern VPCRegion_Cn_north_1 :: VPCRegion
pattern VPCRegion_Cn_north_1 = VPCRegion' "cn-north-1"

pattern VPCRegion_Eu_central_1 :: VPCRegion
pattern VPCRegion_Eu_central_1 = VPCRegion' "eu-central-1"

pattern VPCRegion_Eu_central_2 :: VPCRegion
pattern VPCRegion_Eu_central_2 = VPCRegion' "eu-central-2"

pattern VPCRegion_Eu_north_1 :: VPCRegion
pattern VPCRegion_Eu_north_1 = VPCRegion' "eu-north-1"

pattern VPCRegion_Eu_south_1 :: VPCRegion
pattern VPCRegion_Eu_south_1 = VPCRegion' "eu-south-1"

pattern VPCRegion_Eu_south_2 :: VPCRegion
pattern VPCRegion_Eu_south_2 = VPCRegion' "eu-south-2"

pattern VPCRegion_Eu_west_1 :: VPCRegion
pattern VPCRegion_Eu_west_1 = VPCRegion' "eu-west-1"

pattern VPCRegion_Eu_west_2 :: VPCRegion
pattern VPCRegion_Eu_west_2 = VPCRegion' "eu-west-2"

pattern VPCRegion_Eu_west_3 :: VPCRegion
pattern VPCRegion_Eu_west_3 = VPCRegion' "eu-west-3"

pattern VPCRegion_Me_central_1 :: VPCRegion
pattern VPCRegion_Me_central_1 = VPCRegion' "me-central-1"

pattern VPCRegion_Me_south_1 :: VPCRegion
pattern VPCRegion_Me_south_1 = VPCRegion' "me-south-1"

pattern VPCRegion_Sa_east_1 :: VPCRegion
pattern VPCRegion_Sa_east_1 = VPCRegion' "sa-east-1"

pattern VPCRegion_Us_east_1 :: VPCRegion
pattern VPCRegion_Us_east_1 = VPCRegion' "us-east-1"

pattern VPCRegion_Us_east_2 :: VPCRegion
pattern VPCRegion_Us_east_2 = VPCRegion' "us-east-2"

pattern VPCRegion_Us_gov_east_1 :: VPCRegion
pattern VPCRegion_Us_gov_east_1 = VPCRegion' "us-gov-east-1"

pattern VPCRegion_Us_gov_west_1 :: VPCRegion
pattern VPCRegion_Us_gov_west_1 = VPCRegion' "us-gov-west-1"

pattern VPCRegion_Us_iso_east_1 :: VPCRegion
pattern VPCRegion_Us_iso_east_1 = VPCRegion' "us-iso-east-1"

pattern VPCRegion_Us_iso_west_1 :: VPCRegion
pattern VPCRegion_Us_iso_west_1 = VPCRegion' "us-iso-west-1"

pattern VPCRegion_Us_isob_east_1 :: VPCRegion
pattern VPCRegion_Us_isob_east_1 = VPCRegion' "us-isob-east-1"

pattern VPCRegion_Us_west_1 :: VPCRegion
pattern VPCRegion_Us_west_1 = VPCRegion' "us-west-1"

pattern VPCRegion_Us_west_2 :: VPCRegion
pattern VPCRegion_Us_west_2 = VPCRegion' "us-west-2"

{-# COMPLETE
  VPCRegion_Af_south_1,
  VPCRegion_Ap_east_1,
  VPCRegion_Ap_northeast_1,
  VPCRegion_Ap_northeast_2,
  VPCRegion_Ap_northeast_3,
  VPCRegion_Ap_south_1,
  VPCRegion_Ap_south_2,
  VPCRegion_Ap_southeast_1,
  VPCRegion_Ap_southeast_2,
  VPCRegion_Ap_southeast_3,
  VPCRegion_Ca_central_1,
  VPCRegion_Cn_north_1,
  VPCRegion_Eu_central_1,
  VPCRegion_Eu_central_2,
  VPCRegion_Eu_north_1,
  VPCRegion_Eu_south_1,
  VPCRegion_Eu_south_2,
  VPCRegion_Eu_west_1,
  VPCRegion_Eu_west_2,
  VPCRegion_Eu_west_3,
  VPCRegion_Me_central_1,
  VPCRegion_Me_south_1,
  VPCRegion_Sa_east_1,
  VPCRegion_Us_east_1,
  VPCRegion_Us_east_2,
  VPCRegion_Us_gov_east_1,
  VPCRegion_Us_gov_west_1,
  VPCRegion_Us_iso_east_1,
  VPCRegion_Us_iso_west_1,
  VPCRegion_Us_isob_east_1,
  VPCRegion_Us_west_1,
  VPCRegion_Us_west_2,
  VPCRegion'
  #-}
