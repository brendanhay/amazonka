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
-- Module      : Amazonka.Braket.Types.InstanceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.InstanceType
  ( InstanceType
      ( ..,
        InstanceType_Ml_c4_2xlarge,
        InstanceType_Ml_c4_4xlarge,
        InstanceType_Ml_c4_8xlarge,
        InstanceType_Ml_c4_xlarge,
        InstanceType_Ml_c5_18xlarge,
        InstanceType_Ml_c5_2xlarge,
        InstanceType_Ml_c5_4xlarge,
        InstanceType_Ml_c5_9xlarge,
        InstanceType_Ml_c5_xlarge,
        InstanceType_Ml_c5n_18xlarge,
        InstanceType_Ml_c5n_2xlarge,
        InstanceType_Ml_c5n_4xlarge,
        InstanceType_Ml_c5n_9xlarge,
        InstanceType_Ml_c5n_xlarge,
        InstanceType_Ml_g4dn_12xlarge,
        InstanceType_Ml_g4dn_16xlarge,
        InstanceType_Ml_g4dn_2xlarge,
        InstanceType_Ml_g4dn_4xlarge,
        InstanceType_Ml_g4dn_8xlarge,
        InstanceType_Ml_g4dn_xlarge,
        InstanceType_Ml_m4_10xlarge,
        InstanceType_Ml_m4_16xlarge,
        InstanceType_Ml_m4_2xlarge,
        InstanceType_Ml_m4_4xlarge,
        InstanceType_Ml_m4_xlarge,
        InstanceType_Ml_m5_12xlarge,
        InstanceType_Ml_m5_24xlarge,
        InstanceType_Ml_m5_2xlarge,
        InstanceType_Ml_m5_4xlarge,
        InstanceType_Ml_m5_large,
        InstanceType_Ml_m5_xlarge,
        InstanceType_Ml_p2_16xlarge,
        InstanceType_Ml_p2_8xlarge,
        InstanceType_Ml_p2_xlarge,
        InstanceType_Ml_p3_16xlarge,
        InstanceType_Ml_p3_2xlarge,
        InstanceType_Ml_p3_8xlarge,
        InstanceType_Ml_p3dn_24xlarge,
        InstanceType_Ml_p4d_24xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceType = InstanceType'
  { fromInstanceType ::
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

pattern InstanceType_Ml_c4_2xlarge :: InstanceType
pattern InstanceType_Ml_c4_2xlarge = InstanceType' "ml.c4.2xlarge"

pattern InstanceType_Ml_c4_4xlarge :: InstanceType
pattern InstanceType_Ml_c4_4xlarge = InstanceType' "ml.c4.4xlarge"

pattern InstanceType_Ml_c4_8xlarge :: InstanceType
pattern InstanceType_Ml_c4_8xlarge = InstanceType' "ml.c4.8xlarge"

pattern InstanceType_Ml_c4_xlarge :: InstanceType
pattern InstanceType_Ml_c4_xlarge = InstanceType' "ml.c4.xlarge"

pattern InstanceType_Ml_c5_18xlarge :: InstanceType
pattern InstanceType_Ml_c5_18xlarge = InstanceType' "ml.c5.18xlarge"

pattern InstanceType_Ml_c5_2xlarge :: InstanceType
pattern InstanceType_Ml_c5_2xlarge = InstanceType' "ml.c5.2xlarge"

pattern InstanceType_Ml_c5_4xlarge :: InstanceType
pattern InstanceType_Ml_c5_4xlarge = InstanceType' "ml.c5.4xlarge"

pattern InstanceType_Ml_c5_9xlarge :: InstanceType
pattern InstanceType_Ml_c5_9xlarge = InstanceType' "ml.c5.9xlarge"

pattern InstanceType_Ml_c5_xlarge :: InstanceType
pattern InstanceType_Ml_c5_xlarge = InstanceType' "ml.c5.xlarge"

pattern InstanceType_Ml_c5n_18xlarge :: InstanceType
pattern InstanceType_Ml_c5n_18xlarge = InstanceType' "ml.c5n.18xlarge"

pattern InstanceType_Ml_c5n_2xlarge :: InstanceType
pattern InstanceType_Ml_c5n_2xlarge = InstanceType' "ml.c5n.2xlarge"

pattern InstanceType_Ml_c5n_4xlarge :: InstanceType
pattern InstanceType_Ml_c5n_4xlarge = InstanceType' "ml.c5n.4xlarge"

pattern InstanceType_Ml_c5n_9xlarge :: InstanceType
pattern InstanceType_Ml_c5n_9xlarge = InstanceType' "ml.c5n.9xlarge"

pattern InstanceType_Ml_c5n_xlarge :: InstanceType
pattern InstanceType_Ml_c5n_xlarge = InstanceType' "ml.c5n.xlarge"

pattern InstanceType_Ml_g4dn_12xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_12xlarge = InstanceType' "ml.g4dn.12xlarge"

pattern InstanceType_Ml_g4dn_16xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_16xlarge = InstanceType' "ml.g4dn.16xlarge"

pattern InstanceType_Ml_g4dn_2xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_2xlarge = InstanceType' "ml.g4dn.2xlarge"

pattern InstanceType_Ml_g4dn_4xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_4xlarge = InstanceType' "ml.g4dn.4xlarge"

pattern InstanceType_Ml_g4dn_8xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_8xlarge = InstanceType' "ml.g4dn.8xlarge"

pattern InstanceType_Ml_g4dn_xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_xlarge = InstanceType' "ml.g4dn.xlarge"

pattern InstanceType_Ml_m4_10xlarge :: InstanceType
pattern InstanceType_Ml_m4_10xlarge = InstanceType' "ml.m4.10xlarge"

pattern InstanceType_Ml_m4_16xlarge :: InstanceType
pattern InstanceType_Ml_m4_16xlarge = InstanceType' "ml.m4.16xlarge"

pattern InstanceType_Ml_m4_2xlarge :: InstanceType
pattern InstanceType_Ml_m4_2xlarge = InstanceType' "ml.m4.2xlarge"

pattern InstanceType_Ml_m4_4xlarge :: InstanceType
pattern InstanceType_Ml_m4_4xlarge = InstanceType' "ml.m4.4xlarge"

pattern InstanceType_Ml_m4_xlarge :: InstanceType
pattern InstanceType_Ml_m4_xlarge = InstanceType' "ml.m4.xlarge"

pattern InstanceType_Ml_m5_12xlarge :: InstanceType
pattern InstanceType_Ml_m5_12xlarge = InstanceType' "ml.m5.12xlarge"

pattern InstanceType_Ml_m5_24xlarge :: InstanceType
pattern InstanceType_Ml_m5_24xlarge = InstanceType' "ml.m5.24xlarge"

pattern InstanceType_Ml_m5_2xlarge :: InstanceType
pattern InstanceType_Ml_m5_2xlarge = InstanceType' "ml.m5.2xlarge"

pattern InstanceType_Ml_m5_4xlarge :: InstanceType
pattern InstanceType_Ml_m5_4xlarge = InstanceType' "ml.m5.4xlarge"

pattern InstanceType_Ml_m5_large :: InstanceType
pattern InstanceType_Ml_m5_large = InstanceType' "ml.m5.large"

pattern InstanceType_Ml_m5_xlarge :: InstanceType
pattern InstanceType_Ml_m5_xlarge = InstanceType' "ml.m5.xlarge"

pattern InstanceType_Ml_p2_16xlarge :: InstanceType
pattern InstanceType_Ml_p2_16xlarge = InstanceType' "ml.p2.16xlarge"

pattern InstanceType_Ml_p2_8xlarge :: InstanceType
pattern InstanceType_Ml_p2_8xlarge = InstanceType' "ml.p2.8xlarge"

pattern InstanceType_Ml_p2_xlarge :: InstanceType
pattern InstanceType_Ml_p2_xlarge = InstanceType' "ml.p2.xlarge"

pattern InstanceType_Ml_p3_16xlarge :: InstanceType
pattern InstanceType_Ml_p3_16xlarge = InstanceType' "ml.p3.16xlarge"

pattern InstanceType_Ml_p3_2xlarge :: InstanceType
pattern InstanceType_Ml_p3_2xlarge = InstanceType' "ml.p3.2xlarge"

pattern InstanceType_Ml_p3_8xlarge :: InstanceType
pattern InstanceType_Ml_p3_8xlarge = InstanceType' "ml.p3.8xlarge"

pattern InstanceType_Ml_p3dn_24xlarge :: InstanceType
pattern InstanceType_Ml_p3dn_24xlarge = InstanceType' "ml.p3dn.24xlarge"

pattern InstanceType_Ml_p4d_24xlarge :: InstanceType
pattern InstanceType_Ml_p4d_24xlarge = InstanceType' "ml.p4d.24xlarge"

{-# COMPLETE
  InstanceType_Ml_c4_2xlarge,
  InstanceType_Ml_c4_4xlarge,
  InstanceType_Ml_c4_8xlarge,
  InstanceType_Ml_c4_xlarge,
  InstanceType_Ml_c5_18xlarge,
  InstanceType_Ml_c5_2xlarge,
  InstanceType_Ml_c5_4xlarge,
  InstanceType_Ml_c5_9xlarge,
  InstanceType_Ml_c5_xlarge,
  InstanceType_Ml_c5n_18xlarge,
  InstanceType_Ml_c5n_2xlarge,
  InstanceType_Ml_c5n_4xlarge,
  InstanceType_Ml_c5n_9xlarge,
  InstanceType_Ml_c5n_xlarge,
  InstanceType_Ml_g4dn_12xlarge,
  InstanceType_Ml_g4dn_16xlarge,
  InstanceType_Ml_g4dn_2xlarge,
  InstanceType_Ml_g4dn_4xlarge,
  InstanceType_Ml_g4dn_8xlarge,
  InstanceType_Ml_g4dn_xlarge,
  InstanceType_Ml_m4_10xlarge,
  InstanceType_Ml_m4_16xlarge,
  InstanceType_Ml_m4_2xlarge,
  InstanceType_Ml_m4_4xlarge,
  InstanceType_Ml_m4_xlarge,
  InstanceType_Ml_m5_12xlarge,
  InstanceType_Ml_m5_24xlarge,
  InstanceType_Ml_m5_2xlarge,
  InstanceType_Ml_m5_4xlarge,
  InstanceType_Ml_m5_large,
  InstanceType_Ml_m5_xlarge,
  InstanceType_Ml_p2_16xlarge,
  InstanceType_Ml_p2_8xlarge,
  InstanceType_Ml_p2_xlarge,
  InstanceType_Ml_p3_16xlarge,
  InstanceType_Ml_p3_2xlarge,
  InstanceType_Ml_p3_8xlarge,
  InstanceType_Ml_p3dn_24xlarge,
  InstanceType_Ml_p4d_24xlarge,
  InstanceType'
  #-}
