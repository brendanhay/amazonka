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
-- Module      : Amazonka.GameLift.Types.EC2InstanceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.EC2InstanceType
  ( EC2InstanceType
      ( ..,
        EC2InstanceType_C3_2xlarge,
        EC2InstanceType_C3_4xlarge,
        EC2InstanceType_C3_8xlarge,
        EC2InstanceType_C3_large,
        EC2InstanceType_C3_xlarge,
        EC2InstanceType_C4_2xlarge,
        EC2InstanceType_C4_4xlarge,
        EC2InstanceType_C4_8xlarge,
        EC2InstanceType_C4_large,
        EC2InstanceType_C4_xlarge,
        EC2InstanceType_C5_12xlarge,
        EC2InstanceType_C5_18xlarge,
        EC2InstanceType_C5_24xlarge,
        EC2InstanceType_C5_2xlarge,
        EC2InstanceType_C5_4xlarge,
        EC2InstanceType_C5_9xlarge,
        EC2InstanceType_C5_large,
        EC2InstanceType_C5_xlarge,
        EC2InstanceType_C5a_12xlarge,
        EC2InstanceType_C5a_16xlarge,
        EC2InstanceType_C5a_24xlarge,
        EC2InstanceType_C5a_2xlarge,
        EC2InstanceType_C5a_4xlarge,
        EC2InstanceType_C5a_8xlarge,
        EC2InstanceType_C5a_large,
        EC2InstanceType_C5a_xlarge,
        EC2InstanceType_C5d_12xlarge,
        EC2InstanceType_C5d_18xlarge,
        EC2InstanceType_C5d_24xlarge,
        EC2InstanceType_C5d_2xlarge,
        EC2InstanceType_C5d_4xlarge,
        EC2InstanceType_C5d_9xlarge,
        EC2InstanceType_C5d_large,
        EC2InstanceType_C5d_xlarge,
        EC2InstanceType_C6a_12xlarge,
        EC2InstanceType_C6a_16xlarge,
        EC2InstanceType_C6a_24xlarge,
        EC2InstanceType_C6a_2xlarge,
        EC2InstanceType_C6a_4xlarge,
        EC2InstanceType_C6a_8xlarge,
        EC2InstanceType_C6a_large,
        EC2InstanceType_C6a_xlarge,
        EC2InstanceType_C6i_12xlarge,
        EC2InstanceType_C6i_16xlarge,
        EC2InstanceType_C6i_24xlarge,
        EC2InstanceType_C6i_2xlarge,
        EC2InstanceType_C6i_4xlarge,
        EC2InstanceType_C6i_8xlarge,
        EC2InstanceType_C6i_large,
        EC2InstanceType_C6i_xlarge,
        EC2InstanceType_M3_2xlarge,
        EC2InstanceType_M3_large,
        EC2InstanceType_M3_medium,
        EC2InstanceType_M3_xlarge,
        EC2InstanceType_M4_10xlarge,
        EC2InstanceType_M4_2xlarge,
        EC2InstanceType_M4_4xlarge,
        EC2InstanceType_M4_large,
        EC2InstanceType_M4_xlarge,
        EC2InstanceType_M5_12xlarge,
        EC2InstanceType_M5_16xlarge,
        EC2InstanceType_M5_24xlarge,
        EC2InstanceType_M5_2xlarge,
        EC2InstanceType_M5_4xlarge,
        EC2InstanceType_M5_8xlarge,
        EC2InstanceType_M5_large,
        EC2InstanceType_M5_xlarge,
        EC2InstanceType_M5a_12xlarge,
        EC2InstanceType_M5a_16xlarge,
        EC2InstanceType_M5a_24xlarge,
        EC2InstanceType_M5a_2xlarge,
        EC2InstanceType_M5a_4xlarge,
        EC2InstanceType_M5a_8xlarge,
        EC2InstanceType_M5a_large,
        EC2InstanceType_M5a_xlarge,
        EC2InstanceType_R3_2xlarge,
        EC2InstanceType_R3_4xlarge,
        EC2InstanceType_R3_8xlarge,
        EC2InstanceType_R3_large,
        EC2InstanceType_R3_xlarge,
        EC2InstanceType_R4_16xlarge,
        EC2InstanceType_R4_2xlarge,
        EC2InstanceType_R4_4xlarge,
        EC2InstanceType_R4_8xlarge,
        EC2InstanceType_R4_large,
        EC2InstanceType_R4_xlarge,
        EC2InstanceType_R5_12xlarge,
        EC2InstanceType_R5_16xlarge,
        EC2InstanceType_R5_24xlarge,
        EC2InstanceType_R5_2xlarge,
        EC2InstanceType_R5_4xlarge,
        EC2InstanceType_R5_8xlarge,
        EC2InstanceType_R5_large,
        EC2InstanceType_R5_xlarge,
        EC2InstanceType_R5a_12xlarge,
        EC2InstanceType_R5a_16xlarge,
        EC2InstanceType_R5a_24xlarge,
        EC2InstanceType_R5a_2xlarge,
        EC2InstanceType_R5a_4xlarge,
        EC2InstanceType_R5a_8xlarge,
        EC2InstanceType_R5a_large,
        EC2InstanceType_R5a_xlarge,
        EC2InstanceType_R5d_12xlarge,
        EC2InstanceType_R5d_16xlarge,
        EC2InstanceType_R5d_24xlarge,
        EC2InstanceType_R5d_2xlarge,
        EC2InstanceType_R5d_4xlarge,
        EC2InstanceType_R5d_8xlarge,
        EC2InstanceType_R5d_large,
        EC2InstanceType_R5d_xlarge,
        EC2InstanceType_T2_large,
        EC2InstanceType_T2_medium,
        EC2InstanceType_T2_micro,
        EC2InstanceType_T2_small
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EC2InstanceType = EC2InstanceType'
  { fromEC2InstanceType ::
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

pattern EC2InstanceType_C3_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C3_2xlarge = EC2InstanceType' "c3.2xlarge"

pattern EC2InstanceType_C3_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C3_4xlarge = EC2InstanceType' "c3.4xlarge"

pattern EC2InstanceType_C3_8xlarge :: EC2InstanceType
pattern EC2InstanceType_C3_8xlarge = EC2InstanceType' "c3.8xlarge"

pattern EC2InstanceType_C3_large :: EC2InstanceType
pattern EC2InstanceType_C3_large = EC2InstanceType' "c3.large"

pattern EC2InstanceType_C3_xlarge :: EC2InstanceType
pattern EC2InstanceType_C3_xlarge = EC2InstanceType' "c3.xlarge"

pattern EC2InstanceType_C4_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C4_2xlarge = EC2InstanceType' "c4.2xlarge"

pattern EC2InstanceType_C4_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C4_4xlarge = EC2InstanceType' "c4.4xlarge"

pattern EC2InstanceType_C4_8xlarge :: EC2InstanceType
pattern EC2InstanceType_C4_8xlarge = EC2InstanceType' "c4.8xlarge"

pattern EC2InstanceType_C4_large :: EC2InstanceType
pattern EC2InstanceType_C4_large = EC2InstanceType' "c4.large"

pattern EC2InstanceType_C4_xlarge :: EC2InstanceType
pattern EC2InstanceType_C4_xlarge = EC2InstanceType' "c4.xlarge"

pattern EC2InstanceType_C5_12xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_12xlarge = EC2InstanceType' "c5.12xlarge"

pattern EC2InstanceType_C5_18xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_18xlarge = EC2InstanceType' "c5.18xlarge"

pattern EC2InstanceType_C5_24xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_24xlarge = EC2InstanceType' "c5.24xlarge"

pattern EC2InstanceType_C5_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_2xlarge = EC2InstanceType' "c5.2xlarge"

pattern EC2InstanceType_C5_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_4xlarge = EC2InstanceType' "c5.4xlarge"

pattern EC2InstanceType_C5_9xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_9xlarge = EC2InstanceType' "c5.9xlarge"

pattern EC2InstanceType_C5_large :: EC2InstanceType
pattern EC2InstanceType_C5_large = EC2InstanceType' "c5.large"

pattern EC2InstanceType_C5_xlarge :: EC2InstanceType
pattern EC2InstanceType_C5_xlarge = EC2InstanceType' "c5.xlarge"

pattern EC2InstanceType_C5a_12xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_12xlarge = EC2InstanceType' "c5a.12xlarge"

pattern EC2InstanceType_C5a_16xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_16xlarge = EC2InstanceType' "c5a.16xlarge"

pattern EC2InstanceType_C5a_24xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_24xlarge = EC2InstanceType' "c5a.24xlarge"

pattern EC2InstanceType_C5a_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_2xlarge = EC2InstanceType' "c5a.2xlarge"

pattern EC2InstanceType_C5a_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_4xlarge = EC2InstanceType' "c5a.4xlarge"

pattern EC2InstanceType_C5a_8xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_8xlarge = EC2InstanceType' "c5a.8xlarge"

pattern EC2InstanceType_C5a_large :: EC2InstanceType
pattern EC2InstanceType_C5a_large = EC2InstanceType' "c5a.large"

pattern EC2InstanceType_C5a_xlarge :: EC2InstanceType
pattern EC2InstanceType_C5a_xlarge = EC2InstanceType' "c5a.xlarge"

pattern EC2InstanceType_C5d_12xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_12xlarge = EC2InstanceType' "c5d.12xlarge"

pattern EC2InstanceType_C5d_18xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_18xlarge = EC2InstanceType' "c5d.18xlarge"

pattern EC2InstanceType_C5d_24xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_24xlarge = EC2InstanceType' "c5d.24xlarge"

pattern EC2InstanceType_C5d_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_2xlarge = EC2InstanceType' "c5d.2xlarge"

pattern EC2InstanceType_C5d_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_4xlarge = EC2InstanceType' "c5d.4xlarge"

pattern EC2InstanceType_C5d_9xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_9xlarge = EC2InstanceType' "c5d.9xlarge"

pattern EC2InstanceType_C5d_large :: EC2InstanceType
pattern EC2InstanceType_C5d_large = EC2InstanceType' "c5d.large"

pattern EC2InstanceType_C5d_xlarge :: EC2InstanceType
pattern EC2InstanceType_C5d_xlarge = EC2InstanceType' "c5d.xlarge"

pattern EC2InstanceType_C6a_12xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_12xlarge = EC2InstanceType' "c6a.12xlarge"

pattern EC2InstanceType_C6a_16xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_16xlarge = EC2InstanceType' "c6a.16xlarge"

pattern EC2InstanceType_C6a_24xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_24xlarge = EC2InstanceType' "c6a.24xlarge"

pattern EC2InstanceType_C6a_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_2xlarge = EC2InstanceType' "c6a.2xlarge"

pattern EC2InstanceType_C6a_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_4xlarge = EC2InstanceType' "c6a.4xlarge"

pattern EC2InstanceType_C6a_8xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_8xlarge = EC2InstanceType' "c6a.8xlarge"

pattern EC2InstanceType_C6a_large :: EC2InstanceType
pattern EC2InstanceType_C6a_large = EC2InstanceType' "c6a.large"

pattern EC2InstanceType_C6a_xlarge :: EC2InstanceType
pattern EC2InstanceType_C6a_xlarge = EC2InstanceType' "c6a.xlarge"

pattern EC2InstanceType_C6i_12xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_12xlarge = EC2InstanceType' "c6i.12xlarge"

pattern EC2InstanceType_C6i_16xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_16xlarge = EC2InstanceType' "c6i.16xlarge"

pattern EC2InstanceType_C6i_24xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_24xlarge = EC2InstanceType' "c6i.24xlarge"

pattern EC2InstanceType_C6i_2xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_2xlarge = EC2InstanceType' "c6i.2xlarge"

pattern EC2InstanceType_C6i_4xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_4xlarge = EC2InstanceType' "c6i.4xlarge"

pattern EC2InstanceType_C6i_8xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_8xlarge = EC2InstanceType' "c6i.8xlarge"

pattern EC2InstanceType_C6i_large :: EC2InstanceType
pattern EC2InstanceType_C6i_large = EC2InstanceType' "c6i.large"

pattern EC2InstanceType_C6i_xlarge :: EC2InstanceType
pattern EC2InstanceType_C6i_xlarge = EC2InstanceType' "c6i.xlarge"

pattern EC2InstanceType_M3_2xlarge :: EC2InstanceType
pattern EC2InstanceType_M3_2xlarge = EC2InstanceType' "m3.2xlarge"

pattern EC2InstanceType_M3_large :: EC2InstanceType
pattern EC2InstanceType_M3_large = EC2InstanceType' "m3.large"

pattern EC2InstanceType_M3_medium :: EC2InstanceType
pattern EC2InstanceType_M3_medium = EC2InstanceType' "m3.medium"

pattern EC2InstanceType_M3_xlarge :: EC2InstanceType
pattern EC2InstanceType_M3_xlarge = EC2InstanceType' "m3.xlarge"

pattern EC2InstanceType_M4_10xlarge :: EC2InstanceType
pattern EC2InstanceType_M4_10xlarge = EC2InstanceType' "m4.10xlarge"

pattern EC2InstanceType_M4_2xlarge :: EC2InstanceType
pattern EC2InstanceType_M4_2xlarge = EC2InstanceType' "m4.2xlarge"

pattern EC2InstanceType_M4_4xlarge :: EC2InstanceType
pattern EC2InstanceType_M4_4xlarge = EC2InstanceType' "m4.4xlarge"

pattern EC2InstanceType_M4_large :: EC2InstanceType
pattern EC2InstanceType_M4_large = EC2InstanceType' "m4.large"

pattern EC2InstanceType_M4_xlarge :: EC2InstanceType
pattern EC2InstanceType_M4_xlarge = EC2InstanceType' "m4.xlarge"

pattern EC2InstanceType_M5_12xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_12xlarge = EC2InstanceType' "m5.12xlarge"

pattern EC2InstanceType_M5_16xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_16xlarge = EC2InstanceType' "m5.16xlarge"

pattern EC2InstanceType_M5_24xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_24xlarge = EC2InstanceType' "m5.24xlarge"

pattern EC2InstanceType_M5_2xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_2xlarge = EC2InstanceType' "m5.2xlarge"

pattern EC2InstanceType_M5_4xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_4xlarge = EC2InstanceType' "m5.4xlarge"

pattern EC2InstanceType_M5_8xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_8xlarge = EC2InstanceType' "m5.8xlarge"

pattern EC2InstanceType_M5_large :: EC2InstanceType
pattern EC2InstanceType_M5_large = EC2InstanceType' "m5.large"

pattern EC2InstanceType_M5_xlarge :: EC2InstanceType
pattern EC2InstanceType_M5_xlarge = EC2InstanceType' "m5.xlarge"

pattern EC2InstanceType_M5a_12xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_12xlarge = EC2InstanceType' "m5a.12xlarge"

pattern EC2InstanceType_M5a_16xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_16xlarge = EC2InstanceType' "m5a.16xlarge"

pattern EC2InstanceType_M5a_24xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_24xlarge = EC2InstanceType' "m5a.24xlarge"

pattern EC2InstanceType_M5a_2xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_2xlarge = EC2InstanceType' "m5a.2xlarge"

pattern EC2InstanceType_M5a_4xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_4xlarge = EC2InstanceType' "m5a.4xlarge"

pattern EC2InstanceType_M5a_8xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_8xlarge = EC2InstanceType' "m5a.8xlarge"

pattern EC2InstanceType_M5a_large :: EC2InstanceType
pattern EC2InstanceType_M5a_large = EC2InstanceType' "m5a.large"

pattern EC2InstanceType_M5a_xlarge :: EC2InstanceType
pattern EC2InstanceType_M5a_xlarge = EC2InstanceType' "m5a.xlarge"

pattern EC2InstanceType_R3_2xlarge :: EC2InstanceType
pattern EC2InstanceType_R3_2xlarge = EC2InstanceType' "r3.2xlarge"

pattern EC2InstanceType_R3_4xlarge :: EC2InstanceType
pattern EC2InstanceType_R3_4xlarge = EC2InstanceType' "r3.4xlarge"

pattern EC2InstanceType_R3_8xlarge :: EC2InstanceType
pattern EC2InstanceType_R3_8xlarge = EC2InstanceType' "r3.8xlarge"

pattern EC2InstanceType_R3_large :: EC2InstanceType
pattern EC2InstanceType_R3_large = EC2InstanceType' "r3.large"

pattern EC2InstanceType_R3_xlarge :: EC2InstanceType
pattern EC2InstanceType_R3_xlarge = EC2InstanceType' "r3.xlarge"

pattern EC2InstanceType_R4_16xlarge :: EC2InstanceType
pattern EC2InstanceType_R4_16xlarge = EC2InstanceType' "r4.16xlarge"

pattern EC2InstanceType_R4_2xlarge :: EC2InstanceType
pattern EC2InstanceType_R4_2xlarge = EC2InstanceType' "r4.2xlarge"

pattern EC2InstanceType_R4_4xlarge :: EC2InstanceType
pattern EC2InstanceType_R4_4xlarge = EC2InstanceType' "r4.4xlarge"

pattern EC2InstanceType_R4_8xlarge :: EC2InstanceType
pattern EC2InstanceType_R4_8xlarge = EC2InstanceType' "r4.8xlarge"

pattern EC2InstanceType_R4_large :: EC2InstanceType
pattern EC2InstanceType_R4_large = EC2InstanceType' "r4.large"

pattern EC2InstanceType_R4_xlarge :: EC2InstanceType
pattern EC2InstanceType_R4_xlarge = EC2InstanceType' "r4.xlarge"

pattern EC2InstanceType_R5_12xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_12xlarge = EC2InstanceType' "r5.12xlarge"

pattern EC2InstanceType_R5_16xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_16xlarge = EC2InstanceType' "r5.16xlarge"

pattern EC2InstanceType_R5_24xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_24xlarge = EC2InstanceType' "r5.24xlarge"

pattern EC2InstanceType_R5_2xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_2xlarge = EC2InstanceType' "r5.2xlarge"

pattern EC2InstanceType_R5_4xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_4xlarge = EC2InstanceType' "r5.4xlarge"

pattern EC2InstanceType_R5_8xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_8xlarge = EC2InstanceType' "r5.8xlarge"

pattern EC2InstanceType_R5_large :: EC2InstanceType
pattern EC2InstanceType_R5_large = EC2InstanceType' "r5.large"

pattern EC2InstanceType_R5_xlarge :: EC2InstanceType
pattern EC2InstanceType_R5_xlarge = EC2InstanceType' "r5.xlarge"

pattern EC2InstanceType_R5a_12xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_12xlarge = EC2InstanceType' "r5a.12xlarge"

pattern EC2InstanceType_R5a_16xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_16xlarge = EC2InstanceType' "r5a.16xlarge"

pattern EC2InstanceType_R5a_24xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_24xlarge = EC2InstanceType' "r5a.24xlarge"

pattern EC2InstanceType_R5a_2xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_2xlarge = EC2InstanceType' "r5a.2xlarge"

pattern EC2InstanceType_R5a_4xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_4xlarge = EC2InstanceType' "r5a.4xlarge"

pattern EC2InstanceType_R5a_8xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_8xlarge = EC2InstanceType' "r5a.8xlarge"

pattern EC2InstanceType_R5a_large :: EC2InstanceType
pattern EC2InstanceType_R5a_large = EC2InstanceType' "r5a.large"

pattern EC2InstanceType_R5a_xlarge :: EC2InstanceType
pattern EC2InstanceType_R5a_xlarge = EC2InstanceType' "r5a.xlarge"

pattern EC2InstanceType_R5d_12xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_12xlarge = EC2InstanceType' "r5d.12xlarge"

pattern EC2InstanceType_R5d_16xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_16xlarge = EC2InstanceType' "r5d.16xlarge"

pattern EC2InstanceType_R5d_24xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_24xlarge = EC2InstanceType' "r5d.24xlarge"

pattern EC2InstanceType_R5d_2xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_2xlarge = EC2InstanceType' "r5d.2xlarge"

pattern EC2InstanceType_R5d_4xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_4xlarge = EC2InstanceType' "r5d.4xlarge"

pattern EC2InstanceType_R5d_8xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_8xlarge = EC2InstanceType' "r5d.8xlarge"

pattern EC2InstanceType_R5d_large :: EC2InstanceType
pattern EC2InstanceType_R5d_large = EC2InstanceType' "r5d.large"

pattern EC2InstanceType_R5d_xlarge :: EC2InstanceType
pattern EC2InstanceType_R5d_xlarge = EC2InstanceType' "r5d.xlarge"

pattern EC2InstanceType_T2_large :: EC2InstanceType
pattern EC2InstanceType_T2_large = EC2InstanceType' "t2.large"

pattern EC2InstanceType_T2_medium :: EC2InstanceType
pattern EC2InstanceType_T2_medium = EC2InstanceType' "t2.medium"

pattern EC2InstanceType_T2_micro :: EC2InstanceType
pattern EC2InstanceType_T2_micro = EC2InstanceType' "t2.micro"

pattern EC2InstanceType_T2_small :: EC2InstanceType
pattern EC2InstanceType_T2_small = EC2InstanceType' "t2.small"

{-# COMPLETE
  EC2InstanceType_C3_2xlarge,
  EC2InstanceType_C3_4xlarge,
  EC2InstanceType_C3_8xlarge,
  EC2InstanceType_C3_large,
  EC2InstanceType_C3_xlarge,
  EC2InstanceType_C4_2xlarge,
  EC2InstanceType_C4_4xlarge,
  EC2InstanceType_C4_8xlarge,
  EC2InstanceType_C4_large,
  EC2InstanceType_C4_xlarge,
  EC2InstanceType_C5_12xlarge,
  EC2InstanceType_C5_18xlarge,
  EC2InstanceType_C5_24xlarge,
  EC2InstanceType_C5_2xlarge,
  EC2InstanceType_C5_4xlarge,
  EC2InstanceType_C5_9xlarge,
  EC2InstanceType_C5_large,
  EC2InstanceType_C5_xlarge,
  EC2InstanceType_C5a_12xlarge,
  EC2InstanceType_C5a_16xlarge,
  EC2InstanceType_C5a_24xlarge,
  EC2InstanceType_C5a_2xlarge,
  EC2InstanceType_C5a_4xlarge,
  EC2InstanceType_C5a_8xlarge,
  EC2InstanceType_C5a_large,
  EC2InstanceType_C5a_xlarge,
  EC2InstanceType_C5d_12xlarge,
  EC2InstanceType_C5d_18xlarge,
  EC2InstanceType_C5d_24xlarge,
  EC2InstanceType_C5d_2xlarge,
  EC2InstanceType_C5d_4xlarge,
  EC2InstanceType_C5d_9xlarge,
  EC2InstanceType_C5d_large,
  EC2InstanceType_C5d_xlarge,
  EC2InstanceType_C6a_12xlarge,
  EC2InstanceType_C6a_16xlarge,
  EC2InstanceType_C6a_24xlarge,
  EC2InstanceType_C6a_2xlarge,
  EC2InstanceType_C6a_4xlarge,
  EC2InstanceType_C6a_8xlarge,
  EC2InstanceType_C6a_large,
  EC2InstanceType_C6a_xlarge,
  EC2InstanceType_C6i_12xlarge,
  EC2InstanceType_C6i_16xlarge,
  EC2InstanceType_C6i_24xlarge,
  EC2InstanceType_C6i_2xlarge,
  EC2InstanceType_C6i_4xlarge,
  EC2InstanceType_C6i_8xlarge,
  EC2InstanceType_C6i_large,
  EC2InstanceType_C6i_xlarge,
  EC2InstanceType_M3_2xlarge,
  EC2InstanceType_M3_large,
  EC2InstanceType_M3_medium,
  EC2InstanceType_M3_xlarge,
  EC2InstanceType_M4_10xlarge,
  EC2InstanceType_M4_2xlarge,
  EC2InstanceType_M4_4xlarge,
  EC2InstanceType_M4_large,
  EC2InstanceType_M4_xlarge,
  EC2InstanceType_M5_12xlarge,
  EC2InstanceType_M5_16xlarge,
  EC2InstanceType_M5_24xlarge,
  EC2InstanceType_M5_2xlarge,
  EC2InstanceType_M5_4xlarge,
  EC2InstanceType_M5_8xlarge,
  EC2InstanceType_M5_large,
  EC2InstanceType_M5_xlarge,
  EC2InstanceType_M5a_12xlarge,
  EC2InstanceType_M5a_16xlarge,
  EC2InstanceType_M5a_24xlarge,
  EC2InstanceType_M5a_2xlarge,
  EC2InstanceType_M5a_4xlarge,
  EC2InstanceType_M5a_8xlarge,
  EC2InstanceType_M5a_large,
  EC2InstanceType_M5a_xlarge,
  EC2InstanceType_R3_2xlarge,
  EC2InstanceType_R3_4xlarge,
  EC2InstanceType_R3_8xlarge,
  EC2InstanceType_R3_large,
  EC2InstanceType_R3_xlarge,
  EC2InstanceType_R4_16xlarge,
  EC2InstanceType_R4_2xlarge,
  EC2InstanceType_R4_4xlarge,
  EC2InstanceType_R4_8xlarge,
  EC2InstanceType_R4_large,
  EC2InstanceType_R4_xlarge,
  EC2InstanceType_R5_12xlarge,
  EC2InstanceType_R5_16xlarge,
  EC2InstanceType_R5_24xlarge,
  EC2InstanceType_R5_2xlarge,
  EC2InstanceType_R5_4xlarge,
  EC2InstanceType_R5_8xlarge,
  EC2InstanceType_R5_large,
  EC2InstanceType_R5_xlarge,
  EC2InstanceType_R5a_12xlarge,
  EC2InstanceType_R5a_16xlarge,
  EC2InstanceType_R5a_24xlarge,
  EC2InstanceType_R5a_2xlarge,
  EC2InstanceType_R5a_4xlarge,
  EC2InstanceType_R5a_8xlarge,
  EC2InstanceType_R5a_large,
  EC2InstanceType_R5a_xlarge,
  EC2InstanceType_R5d_12xlarge,
  EC2InstanceType_R5d_16xlarge,
  EC2InstanceType_R5d_24xlarge,
  EC2InstanceType_R5d_2xlarge,
  EC2InstanceType_R5d_4xlarge,
  EC2InstanceType_R5d_8xlarge,
  EC2InstanceType_R5d_large,
  EC2InstanceType_R5d_xlarge,
  EC2InstanceType_T2_large,
  EC2InstanceType_T2_medium,
  EC2InstanceType_T2_micro,
  EC2InstanceType_T2_small,
  EC2InstanceType'
  #-}
