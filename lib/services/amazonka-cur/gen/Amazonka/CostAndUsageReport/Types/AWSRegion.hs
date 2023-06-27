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
-- Module      : Amazonka.CostAndUsageReport.Types.AWSRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostAndUsageReport.Types.AWSRegion
  ( AWSRegion
      ( ..,
        AWSRegion_Af_south_1,
        AWSRegion_Ap_east_1,
        AWSRegion_Ap_northeast_1,
        AWSRegion_Ap_northeast_2,
        AWSRegion_Ap_northeast_3,
        AWSRegion_Ap_south_1,
        AWSRegion_Ap_south_2,
        AWSRegion_Ap_southeast_1,
        AWSRegion_Ap_southeast_2,
        AWSRegion_Ap_southeast_3,
        AWSRegion_Ca_central_1,
        AWSRegion_Cn_north_1,
        AWSRegion_Cn_northwest_1,
        AWSRegion_Eu_central_1,
        AWSRegion_Eu_central_2,
        AWSRegion_Eu_north_1,
        AWSRegion_Eu_south_1,
        AWSRegion_Eu_south_2,
        AWSRegion_Eu_west_1,
        AWSRegion_Eu_west_2,
        AWSRegion_Eu_west_3,
        AWSRegion_Me_central_1,
        AWSRegion_Me_south_1,
        AWSRegion_Sa_east_1,
        AWSRegion_Us_east_1,
        AWSRegion_Us_east_2,
        AWSRegion_Us_west_1,
        AWSRegion_Us_west_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The region of the S3 bucket that AWS delivers the report into.
newtype AWSRegion = AWSRegion'
  { fromAWSRegion ::
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

pattern AWSRegion_Af_south_1 :: AWSRegion
pattern AWSRegion_Af_south_1 = AWSRegion' "af-south-1"

pattern AWSRegion_Ap_east_1 :: AWSRegion
pattern AWSRegion_Ap_east_1 = AWSRegion' "ap-east-1"

pattern AWSRegion_Ap_northeast_1 :: AWSRegion
pattern AWSRegion_Ap_northeast_1 = AWSRegion' "ap-northeast-1"

pattern AWSRegion_Ap_northeast_2 :: AWSRegion
pattern AWSRegion_Ap_northeast_2 = AWSRegion' "ap-northeast-2"

pattern AWSRegion_Ap_northeast_3 :: AWSRegion
pattern AWSRegion_Ap_northeast_3 = AWSRegion' "ap-northeast-3"

pattern AWSRegion_Ap_south_1 :: AWSRegion
pattern AWSRegion_Ap_south_1 = AWSRegion' "ap-south-1"

pattern AWSRegion_Ap_south_2 :: AWSRegion
pattern AWSRegion_Ap_south_2 = AWSRegion' "ap-south-2"

pattern AWSRegion_Ap_southeast_1 :: AWSRegion
pattern AWSRegion_Ap_southeast_1 = AWSRegion' "ap-southeast-1"

pattern AWSRegion_Ap_southeast_2 :: AWSRegion
pattern AWSRegion_Ap_southeast_2 = AWSRegion' "ap-southeast-2"

pattern AWSRegion_Ap_southeast_3 :: AWSRegion
pattern AWSRegion_Ap_southeast_3 = AWSRegion' "ap-southeast-3"

pattern AWSRegion_Ca_central_1 :: AWSRegion
pattern AWSRegion_Ca_central_1 = AWSRegion' "ca-central-1"

pattern AWSRegion_Cn_north_1 :: AWSRegion
pattern AWSRegion_Cn_north_1 = AWSRegion' "cn-north-1"

pattern AWSRegion_Cn_northwest_1 :: AWSRegion
pattern AWSRegion_Cn_northwest_1 = AWSRegion' "cn-northwest-1"

pattern AWSRegion_Eu_central_1 :: AWSRegion
pattern AWSRegion_Eu_central_1 = AWSRegion' "eu-central-1"

pattern AWSRegion_Eu_central_2 :: AWSRegion
pattern AWSRegion_Eu_central_2 = AWSRegion' "eu-central-2"

pattern AWSRegion_Eu_north_1 :: AWSRegion
pattern AWSRegion_Eu_north_1 = AWSRegion' "eu-north-1"

pattern AWSRegion_Eu_south_1 :: AWSRegion
pattern AWSRegion_Eu_south_1 = AWSRegion' "eu-south-1"

pattern AWSRegion_Eu_south_2 :: AWSRegion
pattern AWSRegion_Eu_south_2 = AWSRegion' "eu-south-2"

pattern AWSRegion_Eu_west_1 :: AWSRegion
pattern AWSRegion_Eu_west_1 = AWSRegion' "eu-west-1"

pattern AWSRegion_Eu_west_2 :: AWSRegion
pattern AWSRegion_Eu_west_2 = AWSRegion' "eu-west-2"

pattern AWSRegion_Eu_west_3 :: AWSRegion
pattern AWSRegion_Eu_west_3 = AWSRegion' "eu-west-3"

pattern AWSRegion_Me_central_1 :: AWSRegion
pattern AWSRegion_Me_central_1 = AWSRegion' "me-central-1"

pattern AWSRegion_Me_south_1 :: AWSRegion
pattern AWSRegion_Me_south_1 = AWSRegion' "me-south-1"

pattern AWSRegion_Sa_east_1 :: AWSRegion
pattern AWSRegion_Sa_east_1 = AWSRegion' "sa-east-1"

pattern AWSRegion_Us_east_1 :: AWSRegion
pattern AWSRegion_Us_east_1 = AWSRegion' "us-east-1"

pattern AWSRegion_Us_east_2 :: AWSRegion
pattern AWSRegion_Us_east_2 = AWSRegion' "us-east-2"

pattern AWSRegion_Us_west_1 :: AWSRegion
pattern AWSRegion_Us_west_1 = AWSRegion' "us-west-1"

pattern AWSRegion_Us_west_2 :: AWSRegion
pattern AWSRegion_Us_west_2 = AWSRegion' "us-west-2"

{-# COMPLETE
  AWSRegion_Af_south_1,
  AWSRegion_Ap_east_1,
  AWSRegion_Ap_northeast_1,
  AWSRegion_Ap_northeast_2,
  AWSRegion_Ap_northeast_3,
  AWSRegion_Ap_south_1,
  AWSRegion_Ap_south_2,
  AWSRegion_Ap_southeast_1,
  AWSRegion_Ap_southeast_2,
  AWSRegion_Ap_southeast_3,
  AWSRegion_Ca_central_1,
  AWSRegion_Cn_north_1,
  AWSRegion_Cn_northwest_1,
  AWSRegion_Eu_central_1,
  AWSRegion_Eu_central_2,
  AWSRegion_Eu_north_1,
  AWSRegion_Eu_south_1,
  AWSRegion_Eu_south_2,
  AWSRegion_Eu_west_1,
  AWSRegion_Eu_west_2,
  AWSRegion_Eu_west_3,
  AWSRegion_Me_central_1,
  AWSRegion_Me_south_1,
  AWSRegion_Sa_east_1,
  AWSRegion_Us_east_1,
  AWSRegion_Us_east_2,
  AWSRegion_Us_west_1,
  AWSRegion_Us_west_2,
  AWSRegion'
  #-}
