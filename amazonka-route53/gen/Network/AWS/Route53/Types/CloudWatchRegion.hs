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
-- Module      : Network.AWS.Route53.Types.CloudWatchRegion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.CloudWatchRegion
  ( CloudWatchRegion
      ( ..,
        CloudWatchRegion_Af_south_1,
        CloudWatchRegion_Ap_east_1,
        CloudWatchRegion_Ap_northeast_1,
        CloudWatchRegion_Ap_northeast_2,
        CloudWatchRegion_Ap_northeast_3,
        CloudWatchRegion_Ap_south_1,
        CloudWatchRegion_Ap_southeast_1,
        CloudWatchRegion_Ap_southeast_2,
        CloudWatchRegion_Ca_central_1,
        CloudWatchRegion_Cn_north_1,
        CloudWatchRegion_Cn_northwest_1,
        CloudWatchRegion_Eu_central_1,
        CloudWatchRegion_Eu_north_1,
        CloudWatchRegion_Eu_south_1,
        CloudWatchRegion_Eu_west_1,
        CloudWatchRegion_Eu_west_2,
        CloudWatchRegion_Eu_west_3,
        CloudWatchRegion_Me_south_1,
        CloudWatchRegion_Sa_east_1,
        CloudWatchRegion_Us_east_1,
        CloudWatchRegion_Us_east_2,
        CloudWatchRegion_Us_gov_east_1,
        CloudWatchRegion_Us_gov_west_1,
        CloudWatchRegion_Us_iso_east_1,
        CloudWatchRegion_Us_isob_east_1,
        CloudWatchRegion_Us_west_1,
        CloudWatchRegion_Us_west_2
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype CloudWatchRegion = CloudWatchRegion'
  { fromCloudWatchRegion ::
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

pattern CloudWatchRegion_Af_south_1 :: CloudWatchRegion
pattern CloudWatchRegion_Af_south_1 = CloudWatchRegion' "af-south-1"

pattern CloudWatchRegion_Ap_east_1 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_east_1 = CloudWatchRegion' "ap-east-1"

pattern CloudWatchRegion_Ap_northeast_1 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_northeast_1 = CloudWatchRegion' "ap-northeast-1"

pattern CloudWatchRegion_Ap_northeast_2 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_northeast_2 = CloudWatchRegion' "ap-northeast-2"

pattern CloudWatchRegion_Ap_northeast_3 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_northeast_3 = CloudWatchRegion' "ap-northeast-3"

pattern CloudWatchRegion_Ap_south_1 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_south_1 = CloudWatchRegion' "ap-south-1"

pattern CloudWatchRegion_Ap_southeast_1 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_southeast_1 = CloudWatchRegion' "ap-southeast-1"

pattern CloudWatchRegion_Ap_southeast_2 :: CloudWatchRegion
pattern CloudWatchRegion_Ap_southeast_2 = CloudWatchRegion' "ap-southeast-2"

pattern CloudWatchRegion_Ca_central_1 :: CloudWatchRegion
pattern CloudWatchRegion_Ca_central_1 = CloudWatchRegion' "ca-central-1"

pattern CloudWatchRegion_Cn_north_1 :: CloudWatchRegion
pattern CloudWatchRegion_Cn_north_1 = CloudWatchRegion' "cn-north-1"

pattern CloudWatchRegion_Cn_northwest_1 :: CloudWatchRegion
pattern CloudWatchRegion_Cn_northwest_1 = CloudWatchRegion' "cn-northwest-1"

pattern CloudWatchRegion_Eu_central_1 :: CloudWatchRegion
pattern CloudWatchRegion_Eu_central_1 = CloudWatchRegion' "eu-central-1"

pattern CloudWatchRegion_Eu_north_1 :: CloudWatchRegion
pattern CloudWatchRegion_Eu_north_1 = CloudWatchRegion' "eu-north-1"

pattern CloudWatchRegion_Eu_south_1 :: CloudWatchRegion
pattern CloudWatchRegion_Eu_south_1 = CloudWatchRegion' "eu-south-1"

pattern CloudWatchRegion_Eu_west_1 :: CloudWatchRegion
pattern CloudWatchRegion_Eu_west_1 = CloudWatchRegion' "eu-west-1"

pattern CloudWatchRegion_Eu_west_2 :: CloudWatchRegion
pattern CloudWatchRegion_Eu_west_2 = CloudWatchRegion' "eu-west-2"

pattern CloudWatchRegion_Eu_west_3 :: CloudWatchRegion
pattern CloudWatchRegion_Eu_west_3 = CloudWatchRegion' "eu-west-3"

pattern CloudWatchRegion_Me_south_1 :: CloudWatchRegion
pattern CloudWatchRegion_Me_south_1 = CloudWatchRegion' "me-south-1"

pattern CloudWatchRegion_Sa_east_1 :: CloudWatchRegion
pattern CloudWatchRegion_Sa_east_1 = CloudWatchRegion' "sa-east-1"

pattern CloudWatchRegion_Us_east_1 :: CloudWatchRegion
pattern CloudWatchRegion_Us_east_1 = CloudWatchRegion' "us-east-1"

pattern CloudWatchRegion_Us_east_2 :: CloudWatchRegion
pattern CloudWatchRegion_Us_east_2 = CloudWatchRegion' "us-east-2"

pattern CloudWatchRegion_Us_gov_east_1 :: CloudWatchRegion
pattern CloudWatchRegion_Us_gov_east_1 = CloudWatchRegion' "us-gov-east-1"

pattern CloudWatchRegion_Us_gov_west_1 :: CloudWatchRegion
pattern CloudWatchRegion_Us_gov_west_1 = CloudWatchRegion' "us-gov-west-1"

pattern CloudWatchRegion_Us_iso_east_1 :: CloudWatchRegion
pattern CloudWatchRegion_Us_iso_east_1 = CloudWatchRegion' "us-iso-east-1"

pattern CloudWatchRegion_Us_isob_east_1 :: CloudWatchRegion
pattern CloudWatchRegion_Us_isob_east_1 = CloudWatchRegion' "us-isob-east-1"

pattern CloudWatchRegion_Us_west_1 :: CloudWatchRegion
pattern CloudWatchRegion_Us_west_1 = CloudWatchRegion' "us-west-1"

pattern CloudWatchRegion_Us_west_2 :: CloudWatchRegion
pattern CloudWatchRegion_Us_west_2 = CloudWatchRegion' "us-west-2"

{-# COMPLETE
  CloudWatchRegion_Af_south_1,
  CloudWatchRegion_Ap_east_1,
  CloudWatchRegion_Ap_northeast_1,
  CloudWatchRegion_Ap_northeast_2,
  CloudWatchRegion_Ap_northeast_3,
  CloudWatchRegion_Ap_south_1,
  CloudWatchRegion_Ap_southeast_1,
  CloudWatchRegion_Ap_southeast_2,
  CloudWatchRegion_Ca_central_1,
  CloudWatchRegion_Cn_north_1,
  CloudWatchRegion_Cn_northwest_1,
  CloudWatchRegion_Eu_central_1,
  CloudWatchRegion_Eu_north_1,
  CloudWatchRegion_Eu_south_1,
  CloudWatchRegion_Eu_west_1,
  CloudWatchRegion_Eu_west_2,
  CloudWatchRegion_Eu_west_3,
  CloudWatchRegion_Me_south_1,
  CloudWatchRegion_Sa_east_1,
  CloudWatchRegion_Us_east_1,
  CloudWatchRegion_Us_east_2,
  CloudWatchRegion_Us_gov_east_1,
  CloudWatchRegion_Us_gov_west_1,
  CloudWatchRegion_Us_iso_east_1,
  CloudWatchRegion_Us_isob_east_1,
  CloudWatchRegion_Us_west_1,
  CloudWatchRegion_Us_west_2,
  CloudWatchRegion'
  #-}
