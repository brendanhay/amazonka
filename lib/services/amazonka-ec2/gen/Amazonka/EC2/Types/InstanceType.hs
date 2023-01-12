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
-- Module      : Amazonka.EC2.Types.InstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceType
  ( InstanceType
      ( ..,
        InstanceType_A1_2xlarge,
        InstanceType_A1_4xlarge,
        InstanceType_A1_large,
        InstanceType_A1_medium,
        InstanceType_A1_metal,
        InstanceType_A1_xlarge,
        InstanceType_C1_medium,
        InstanceType_C1_xlarge,
        InstanceType_C3_2xlarge,
        InstanceType_C3_4xlarge,
        InstanceType_C3_8xlarge,
        InstanceType_C3_large,
        InstanceType_C3_xlarge,
        InstanceType_C4_2xlarge,
        InstanceType_C4_4xlarge,
        InstanceType_C4_8xlarge,
        InstanceType_C4_large,
        InstanceType_C4_xlarge,
        InstanceType_C5_12xlarge,
        InstanceType_C5_18xlarge,
        InstanceType_C5_24xlarge,
        InstanceType_C5_2xlarge,
        InstanceType_C5_4xlarge,
        InstanceType_C5_9xlarge,
        InstanceType_C5_large,
        InstanceType_C5_metal,
        InstanceType_C5_xlarge,
        InstanceType_C5a_12xlarge,
        InstanceType_C5a_16xlarge,
        InstanceType_C5a_24xlarge,
        InstanceType_C5a_2xlarge,
        InstanceType_C5a_4xlarge,
        InstanceType_C5a_8xlarge,
        InstanceType_C5a_large,
        InstanceType_C5a_xlarge,
        InstanceType_C5ad_12xlarge,
        InstanceType_C5ad_16xlarge,
        InstanceType_C5ad_24xlarge,
        InstanceType_C5ad_2xlarge,
        InstanceType_C5ad_4xlarge,
        InstanceType_C5ad_8xlarge,
        InstanceType_C5ad_large,
        InstanceType_C5ad_xlarge,
        InstanceType_C5d_12xlarge,
        InstanceType_C5d_18xlarge,
        InstanceType_C5d_24xlarge,
        InstanceType_C5d_2xlarge,
        InstanceType_C5d_4xlarge,
        InstanceType_C5d_9xlarge,
        InstanceType_C5d_large,
        InstanceType_C5d_metal,
        InstanceType_C5d_xlarge,
        InstanceType_C5n_18xlarge,
        InstanceType_C5n_2xlarge,
        InstanceType_C5n_4xlarge,
        InstanceType_C5n_9xlarge,
        InstanceType_C5n_large,
        InstanceType_C5n_metal,
        InstanceType_C5n_xlarge,
        InstanceType_C6a_12xlarge,
        InstanceType_C6a_16xlarge,
        InstanceType_C6a_24xlarge,
        InstanceType_C6a_2xlarge,
        InstanceType_C6a_32xlarge,
        InstanceType_C6a_48xlarge,
        InstanceType_C6a_4xlarge,
        InstanceType_C6a_8xlarge,
        InstanceType_C6a_large,
        InstanceType_C6a_metal,
        InstanceType_C6a_xlarge,
        InstanceType_C6g_12xlarge,
        InstanceType_C6g_16xlarge,
        InstanceType_C6g_2xlarge,
        InstanceType_C6g_4xlarge,
        InstanceType_C6g_8xlarge,
        InstanceType_C6g_large,
        InstanceType_C6g_medium,
        InstanceType_C6g_metal,
        InstanceType_C6g_xlarge,
        InstanceType_C6gd_12xlarge,
        InstanceType_C6gd_16xlarge,
        InstanceType_C6gd_2xlarge,
        InstanceType_C6gd_4xlarge,
        InstanceType_C6gd_8xlarge,
        InstanceType_C6gd_large,
        InstanceType_C6gd_medium,
        InstanceType_C6gd_metal,
        InstanceType_C6gd_xlarge,
        InstanceType_C6gn_12xlarge,
        InstanceType_C6gn_16xlarge,
        InstanceType_C6gn_2xlarge,
        InstanceType_C6gn_4xlarge,
        InstanceType_C6gn_8xlarge,
        InstanceType_C6gn_large,
        InstanceType_C6gn_medium,
        InstanceType_C6gn_xlarge,
        InstanceType_C6i_12xlarge,
        InstanceType_C6i_16xlarge,
        InstanceType_C6i_24xlarge,
        InstanceType_C6i_2xlarge,
        InstanceType_C6i_32xlarge,
        InstanceType_C6i_4xlarge,
        InstanceType_C6i_8xlarge,
        InstanceType_C6i_large,
        InstanceType_C6i_metal,
        InstanceType_C6i_xlarge,
        InstanceType_C6id_12xlarge,
        InstanceType_C6id_16xlarge,
        InstanceType_C6id_24xlarge,
        InstanceType_C6id_2xlarge,
        InstanceType_C6id_32xlarge,
        InstanceType_C6id_4xlarge,
        InstanceType_C6id_8xlarge,
        InstanceType_C6id_large,
        InstanceType_C6id_metal,
        InstanceType_C6id_xlarge,
        InstanceType_C7g_12xlarge,
        InstanceType_C7g_16xlarge,
        InstanceType_C7g_2xlarge,
        InstanceType_C7g_4xlarge,
        InstanceType_C7g_8xlarge,
        InstanceType_C7g_large,
        InstanceType_C7g_medium,
        InstanceType_C7g_xlarge,
        InstanceType_Cc1_4xlarge,
        InstanceType_Cc2_8xlarge,
        InstanceType_Cg1_4xlarge,
        InstanceType_Cr1_8xlarge,
        InstanceType_D2_2xlarge,
        InstanceType_D2_4xlarge,
        InstanceType_D2_8xlarge,
        InstanceType_D2_xlarge,
        InstanceType_D3_2xlarge,
        InstanceType_D3_4xlarge,
        InstanceType_D3_8xlarge,
        InstanceType_D3_xlarge,
        InstanceType_D3en_12xlarge,
        InstanceType_D3en_2xlarge,
        InstanceType_D3en_4xlarge,
        InstanceType_D3en_6xlarge,
        InstanceType_D3en_8xlarge,
        InstanceType_D3en_xlarge,
        InstanceType_Dl1_24xlarge,
        InstanceType_F1_16xlarge,
        InstanceType_F1_2xlarge,
        InstanceType_F1_4xlarge,
        InstanceType_G2_2xlarge,
        InstanceType_G2_8xlarge,
        InstanceType_G3_16xlarge,
        InstanceType_G3_4xlarge,
        InstanceType_G3_8xlarge,
        InstanceType_G3s_xlarge,
        InstanceType_G4ad_16xlarge,
        InstanceType_G4ad_2xlarge,
        InstanceType_G4ad_4xlarge,
        InstanceType_G4ad_8xlarge,
        InstanceType_G4ad_xlarge,
        InstanceType_G4dn_12xlarge,
        InstanceType_G4dn_16xlarge,
        InstanceType_G4dn_2xlarge,
        InstanceType_G4dn_4xlarge,
        InstanceType_G4dn_8xlarge,
        InstanceType_G4dn_metal,
        InstanceType_G4dn_xlarge,
        InstanceType_G5_12xlarge,
        InstanceType_G5_16xlarge,
        InstanceType_G5_24xlarge,
        InstanceType_G5_2xlarge,
        InstanceType_G5_48xlarge,
        InstanceType_G5_4xlarge,
        InstanceType_G5_8xlarge,
        InstanceType_G5_xlarge,
        InstanceType_G5g_16xlarge,
        InstanceType_G5g_2xlarge,
        InstanceType_G5g_4xlarge,
        InstanceType_G5g_8xlarge,
        InstanceType_G5g_metal,
        InstanceType_G5g_xlarge,
        InstanceType_H1_16xlarge,
        InstanceType_H1_2xlarge,
        InstanceType_H1_4xlarge,
        InstanceType_H1_8xlarge,
        InstanceType_Hi1_4xlarge,
        InstanceType_Hpc6a_48xlarge,
        InstanceType_Hpc6id_32xlarge,
        InstanceType_Hs1_8xlarge,
        InstanceType_I2_2xlarge,
        InstanceType_I2_4xlarge,
        InstanceType_I2_8xlarge,
        InstanceType_I2_xlarge,
        InstanceType_I3_16xlarge,
        InstanceType_I3_2xlarge,
        InstanceType_I3_4xlarge,
        InstanceType_I3_8xlarge,
        InstanceType_I3_large,
        InstanceType_I3_metal,
        InstanceType_I3_xlarge,
        InstanceType_I3en_12xlarge,
        InstanceType_I3en_24xlarge,
        InstanceType_I3en_2xlarge,
        InstanceType_I3en_3xlarge,
        InstanceType_I3en_6xlarge,
        InstanceType_I3en_large,
        InstanceType_I3en_metal,
        InstanceType_I3en_xlarge,
        InstanceType_I4i_16xlarge,
        InstanceType_I4i_2xlarge,
        InstanceType_I4i_32xlarge,
        InstanceType_I4i_4xlarge,
        InstanceType_I4i_8xlarge,
        InstanceType_I4i_large,
        InstanceType_I4i_metal,
        InstanceType_I4i_xlarge,
        InstanceType_Im4gn_16xlarge,
        InstanceType_Im4gn_2xlarge,
        InstanceType_Im4gn_4xlarge,
        InstanceType_Im4gn_8xlarge,
        InstanceType_Im4gn_large,
        InstanceType_Im4gn_xlarge,
        InstanceType_Inf1_24xlarge,
        InstanceType_Inf1_2xlarge,
        InstanceType_Inf1_6xlarge,
        InstanceType_Inf1_xlarge,
        InstanceType_Is4gen_2xlarge,
        InstanceType_Is4gen_4xlarge,
        InstanceType_Is4gen_8xlarge,
        InstanceType_Is4gen_large,
        InstanceType_Is4gen_medium,
        InstanceType_Is4gen_xlarge,
        InstanceType_M1_large,
        InstanceType_M1_medium,
        InstanceType_M1_small,
        InstanceType_M1_xlarge,
        InstanceType_M2_2xlarge,
        InstanceType_M2_4xlarge,
        InstanceType_M2_xlarge,
        InstanceType_M3_2xlarge,
        InstanceType_M3_large,
        InstanceType_M3_medium,
        InstanceType_M3_xlarge,
        InstanceType_M4_10xlarge,
        InstanceType_M4_16xlarge,
        InstanceType_M4_2xlarge,
        InstanceType_M4_4xlarge,
        InstanceType_M4_large,
        InstanceType_M4_xlarge,
        InstanceType_M5_12xlarge,
        InstanceType_M5_16xlarge,
        InstanceType_M5_24xlarge,
        InstanceType_M5_2xlarge,
        InstanceType_M5_4xlarge,
        InstanceType_M5_8xlarge,
        InstanceType_M5_large,
        InstanceType_M5_metal,
        InstanceType_M5_xlarge,
        InstanceType_M5a_12xlarge,
        InstanceType_M5a_16xlarge,
        InstanceType_M5a_24xlarge,
        InstanceType_M5a_2xlarge,
        InstanceType_M5a_4xlarge,
        InstanceType_M5a_8xlarge,
        InstanceType_M5a_large,
        InstanceType_M5a_xlarge,
        InstanceType_M5ad_12xlarge,
        InstanceType_M5ad_16xlarge,
        InstanceType_M5ad_24xlarge,
        InstanceType_M5ad_2xlarge,
        InstanceType_M5ad_4xlarge,
        InstanceType_M5ad_8xlarge,
        InstanceType_M5ad_large,
        InstanceType_M5ad_xlarge,
        InstanceType_M5d_12xlarge,
        InstanceType_M5d_16xlarge,
        InstanceType_M5d_24xlarge,
        InstanceType_M5d_2xlarge,
        InstanceType_M5d_4xlarge,
        InstanceType_M5d_8xlarge,
        InstanceType_M5d_large,
        InstanceType_M5d_metal,
        InstanceType_M5d_xlarge,
        InstanceType_M5dn_12xlarge,
        InstanceType_M5dn_16xlarge,
        InstanceType_M5dn_24xlarge,
        InstanceType_M5dn_2xlarge,
        InstanceType_M5dn_4xlarge,
        InstanceType_M5dn_8xlarge,
        InstanceType_M5dn_large,
        InstanceType_M5dn_metal,
        InstanceType_M5dn_xlarge,
        InstanceType_M5n_12xlarge,
        InstanceType_M5n_16xlarge,
        InstanceType_M5n_24xlarge,
        InstanceType_M5n_2xlarge,
        InstanceType_M5n_4xlarge,
        InstanceType_M5n_8xlarge,
        InstanceType_M5n_large,
        InstanceType_M5n_metal,
        InstanceType_M5n_xlarge,
        InstanceType_M5zn_12xlarge,
        InstanceType_M5zn_2xlarge,
        InstanceType_M5zn_3xlarge,
        InstanceType_M5zn_6xlarge,
        InstanceType_M5zn_large,
        InstanceType_M5zn_metal,
        InstanceType_M5zn_xlarge,
        InstanceType_M6a_12xlarge,
        InstanceType_M6a_16xlarge,
        InstanceType_M6a_24xlarge,
        InstanceType_M6a_2xlarge,
        InstanceType_M6a_32xlarge,
        InstanceType_M6a_48xlarge,
        InstanceType_M6a_4xlarge,
        InstanceType_M6a_8xlarge,
        InstanceType_M6a_large,
        InstanceType_M6a_metal,
        InstanceType_M6a_xlarge,
        InstanceType_M6g_12xlarge,
        InstanceType_M6g_16xlarge,
        InstanceType_M6g_2xlarge,
        InstanceType_M6g_4xlarge,
        InstanceType_M6g_8xlarge,
        InstanceType_M6g_large,
        InstanceType_M6g_medium,
        InstanceType_M6g_metal,
        InstanceType_M6g_xlarge,
        InstanceType_M6gd_12xlarge,
        InstanceType_M6gd_16xlarge,
        InstanceType_M6gd_2xlarge,
        InstanceType_M6gd_4xlarge,
        InstanceType_M6gd_8xlarge,
        InstanceType_M6gd_large,
        InstanceType_M6gd_medium,
        InstanceType_M6gd_metal,
        InstanceType_M6gd_xlarge,
        InstanceType_M6i_12xlarge,
        InstanceType_M6i_16xlarge,
        InstanceType_M6i_24xlarge,
        InstanceType_M6i_2xlarge,
        InstanceType_M6i_32xlarge,
        InstanceType_M6i_4xlarge,
        InstanceType_M6i_8xlarge,
        InstanceType_M6i_large,
        InstanceType_M6i_metal,
        InstanceType_M6i_xlarge,
        InstanceType_M6id_12xlarge,
        InstanceType_M6id_16xlarge,
        InstanceType_M6id_24xlarge,
        InstanceType_M6id_2xlarge,
        InstanceType_M6id_32xlarge,
        InstanceType_M6id_4xlarge,
        InstanceType_M6id_8xlarge,
        InstanceType_M6id_large,
        InstanceType_M6id_metal,
        InstanceType_M6id_xlarge,
        InstanceType_Mac1_metal,
        InstanceType_Mac2_metal,
        InstanceType_P2_16xlarge,
        InstanceType_P2_8xlarge,
        InstanceType_P2_xlarge,
        InstanceType_P3_16xlarge,
        InstanceType_P3_2xlarge,
        InstanceType_P3_8xlarge,
        InstanceType_P3dn_24xlarge,
        InstanceType_P4d_24xlarge,
        InstanceType_P4de_24xlarge,
        InstanceType_R3_2xlarge,
        InstanceType_R3_4xlarge,
        InstanceType_R3_8xlarge,
        InstanceType_R3_large,
        InstanceType_R3_xlarge,
        InstanceType_R4_16xlarge,
        InstanceType_R4_2xlarge,
        InstanceType_R4_4xlarge,
        InstanceType_R4_8xlarge,
        InstanceType_R4_large,
        InstanceType_R4_xlarge,
        InstanceType_R5_12xlarge,
        InstanceType_R5_16xlarge,
        InstanceType_R5_24xlarge,
        InstanceType_R5_2xlarge,
        InstanceType_R5_4xlarge,
        InstanceType_R5_8xlarge,
        InstanceType_R5_large,
        InstanceType_R5_metal,
        InstanceType_R5_xlarge,
        InstanceType_R5a_12xlarge,
        InstanceType_R5a_16xlarge,
        InstanceType_R5a_24xlarge,
        InstanceType_R5a_2xlarge,
        InstanceType_R5a_4xlarge,
        InstanceType_R5a_8xlarge,
        InstanceType_R5a_large,
        InstanceType_R5a_xlarge,
        InstanceType_R5ad_12xlarge,
        InstanceType_R5ad_16xlarge,
        InstanceType_R5ad_24xlarge,
        InstanceType_R5ad_2xlarge,
        InstanceType_R5ad_4xlarge,
        InstanceType_R5ad_8xlarge,
        InstanceType_R5ad_large,
        InstanceType_R5ad_xlarge,
        InstanceType_R5b_12xlarge,
        InstanceType_R5b_16xlarge,
        InstanceType_R5b_24xlarge,
        InstanceType_R5b_2xlarge,
        InstanceType_R5b_4xlarge,
        InstanceType_R5b_8xlarge,
        InstanceType_R5b_large,
        InstanceType_R5b_metal,
        InstanceType_R5b_xlarge,
        InstanceType_R5d_12xlarge,
        InstanceType_R5d_16xlarge,
        InstanceType_R5d_24xlarge,
        InstanceType_R5d_2xlarge,
        InstanceType_R5d_4xlarge,
        InstanceType_R5d_8xlarge,
        InstanceType_R5d_large,
        InstanceType_R5d_metal,
        InstanceType_R5d_xlarge,
        InstanceType_R5dn_12xlarge,
        InstanceType_R5dn_16xlarge,
        InstanceType_R5dn_24xlarge,
        InstanceType_R5dn_2xlarge,
        InstanceType_R5dn_4xlarge,
        InstanceType_R5dn_8xlarge,
        InstanceType_R5dn_large,
        InstanceType_R5dn_metal,
        InstanceType_R5dn_xlarge,
        InstanceType_R5n_12xlarge,
        InstanceType_R5n_16xlarge,
        InstanceType_R5n_24xlarge,
        InstanceType_R5n_2xlarge,
        InstanceType_R5n_4xlarge,
        InstanceType_R5n_8xlarge,
        InstanceType_R5n_large,
        InstanceType_R5n_metal,
        InstanceType_R5n_xlarge,
        InstanceType_R6a_12xlarge,
        InstanceType_R6a_16xlarge,
        InstanceType_R6a_24xlarge,
        InstanceType_R6a_2xlarge,
        InstanceType_R6a_32xlarge,
        InstanceType_R6a_48xlarge,
        InstanceType_R6a_4xlarge,
        InstanceType_R6a_8xlarge,
        InstanceType_R6a_large,
        InstanceType_R6a_metal,
        InstanceType_R6a_xlarge,
        InstanceType_R6g_12xlarge,
        InstanceType_R6g_16xlarge,
        InstanceType_R6g_2xlarge,
        InstanceType_R6g_4xlarge,
        InstanceType_R6g_8xlarge,
        InstanceType_R6g_large,
        InstanceType_R6g_medium,
        InstanceType_R6g_metal,
        InstanceType_R6g_xlarge,
        InstanceType_R6gd_12xlarge,
        InstanceType_R6gd_16xlarge,
        InstanceType_R6gd_2xlarge,
        InstanceType_R6gd_4xlarge,
        InstanceType_R6gd_8xlarge,
        InstanceType_R6gd_large,
        InstanceType_R6gd_medium,
        InstanceType_R6gd_metal,
        InstanceType_R6gd_xlarge,
        InstanceType_R6i_12xlarge,
        InstanceType_R6i_16xlarge,
        InstanceType_R6i_24xlarge,
        InstanceType_R6i_2xlarge,
        InstanceType_R6i_32xlarge,
        InstanceType_R6i_4xlarge,
        InstanceType_R6i_8xlarge,
        InstanceType_R6i_large,
        InstanceType_R6i_metal,
        InstanceType_R6i_xlarge,
        InstanceType_R6id_12xlarge,
        InstanceType_R6id_16xlarge,
        InstanceType_R6id_24xlarge,
        InstanceType_R6id_2xlarge,
        InstanceType_R6id_32xlarge,
        InstanceType_R6id_4xlarge,
        InstanceType_R6id_8xlarge,
        InstanceType_R6id_large,
        InstanceType_R6id_metal,
        InstanceType_R6id_xlarge,
        InstanceType_T1_micro,
        InstanceType_T2_2xlarge,
        InstanceType_T2_large,
        InstanceType_T2_medium,
        InstanceType_T2_micro,
        InstanceType_T2_nano,
        InstanceType_T2_small,
        InstanceType_T2_xlarge,
        InstanceType_T3_2xlarge,
        InstanceType_T3_large,
        InstanceType_T3_medium,
        InstanceType_T3_micro,
        InstanceType_T3_nano,
        InstanceType_T3_small,
        InstanceType_T3_xlarge,
        InstanceType_T3a_2xlarge,
        InstanceType_T3a_large,
        InstanceType_T3a_medium,
        InstanceType_T3a_micro,
        InstanceType_T3a_nano,
        InstanceType_T3a_small,
        InstanceType_T3a_xlarge,
        InstanceType_T4g_2xlarge,
        InstanceType_T4g_large,
        InstanceType_T4g_medium,
        InstanceType_T4g_micro,
        InstanceType_T4g_nano,
        InstanceType_T4g_small,
        InstanceType_T4g_xlarge,
        InstanceType_Trn1_2xlarge,
        InstanceType_Trn1_32xlarge,
        InstanceType_U_12tb1_112xlarge,
        InstanceType_U_12tb1_metal,
        InstanceType_U_18tb1_112xlarge,
        InstanceType_U_18tb1_metal,
        InstanceType_U_24tb1_112xlarge,
        InstanceType_U_24tb1_metal,
        InstanceType_U_3tb1_56xlarge,
        InstanceType_U_6tb1_112xlarge,
        InstanceType_U_6tb1_56xlarge,
        InstanceType_U_6tb1_metal,
        InstanceType_U_9tb1_112xlarge,
        InstanceType_U_9tb1_metal,
        InstanceType_Vt1_24xlarge,
        InstanceType_Vt1_3xlarge,
        InstanceType_Vt1_6xlarge,
        InstanceType_X1_16xlarge,
        InstanceType_X1_32xlarge,
        InstanceType_X1e_16xlarge,
        InstanceType_X1e_2xlarge,
        InstanceType_X1e_32xlarge,
        InstanceType_X1e_4xlarge,
        InstanceType_X1e_8xlarge,
        InstanceType_X1e_xlarge,
        InstanceType_X2gd_12xlarge,
        InstanceType_X2gd_16xlarge,
        InstanceType_X2gd_2xlarge,
        InstanceType_X2gd_4xlarge,
        InstanceType_X2gd_8xlarge,
        InstanceType_X2gd_large,
        InstanceType_X2gd_medium,
        InstanceType_X2gd_metal,
        InstanceType_X2gd_xlarge,
        InstanceType_X2idn_16xlarge,
        InstanceType_X2idn_24xlarge,
        InstanceType_X2idn_32xlarge,
        InstanceType_X2idn_metal,
        InstanceType_X2iedn_16xlarge,
        InstanceType_X2iedn_24xlarge,
        InstanceType_X2iedn_2xlarge,
        InstanceType_X2iedn_32xlarge,
        InstanceType_X2iedn_4xlarge,
        InstanceType_X2iedn_8xlarge,
        InstanceType_X2iedn_metal,
        InstanceType_X2iedn_xlarge,
        InstanceType_X2iezn_12xlarge,
        InstanceType_X2iezn_2xlarge,
        InstanceType_X2iezn_4xlarge,
        InstanceType_X2iezn_6xlarge,
        InstanceType_X2iezn_8xlarge,
        InstanceType_X2iezn_metal,
        InstanceType_Z1d_12xlarge,
        InstanceType_Z1d_2xlarge,
        InstanceType_Z1d_3xlarge,
        InstanceType_Z1d_6xlarge,
        InstanceType_Z1d_large,
        InstanceType_Z1d_metal,
        InstanceType_Z1d_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
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

pattern InstanceType_A1_2xlarge :: InstanceType
pattern InstanceType_A1_2xlarge = InstanceType' "a1.2xlarge"

pattern InstanceType_A1_4xlarge :: InstanceType
pattern InstanceType_A1_4xlarge = InstanceType' "a1.4xlarge"

pattern InstanceType_A1_large :: InstanceType
pattern InstanceType_A1_large = InstanceType' "a1.large"

pattern InstanceType_A1_medium :: InstanceType
pattern InstanceType_A1_medium = InstanceType' "a1.medium"

pattern InstanceType_A1_metal :: InstanceType
pattern InstanceType_A1_metal = InstanceType' "a1.metal"

pattern InstanceType_A1_xlarge :: InstanceType
pattern InstanceType_A1_xlarge = InstanceType' "a1.xlarge"

pattern InstanceType_C1_medium :: InstanceType
pattern InstanceType_C1_medium = InstanceType' "c1.medium"

pattern InstanceType_C1_xlarge :: InstanceType
pattern InstanceType_C1_xlarge = InstanceType' "c1.xlarge"

pattern InstanceType_C3_2xlarge :: InstanceType
pattern InstanceType_C3_2xlarge = InstanceType' "c3.2xlarge"

pattern InstanceType_C3_4xlarge :: InstanceType
pattern InstanceType_C3_4xlarge = InstanceType' "c3.4xlarge"

pattern InstanceType_C3_8xlarge :: InstanceType
pattern InstanceType_C3_8xlarge = InstanceType' "c3.8xlarge"

pattern InstanceType_C3_large :: InstanceType
pattern InstanceType_C3_large = InstanceType' "c3.large"

pattern InstanceType_C3_xlarge :: InstanceType
pattern InstanceType_C3_xlarge = InstanceType' "c3.xlarge"

pattern InstanceType_C4_2xlarge :: InstanceType
pattern InstanceType_C4_2xlarge = InstanceType' "c4.2xlarge"

pattern InstanceType_C4_4xlarge :: InstanceType
pattern InstanceType_C4_4xlarge = InstanceType' "c4.4xlarge"

pattern InstanceType_C4_8xlarge :: InstanceType
pattern InstanceType_C4_8xlarge = InstanceType' "c4.8xlarge"

pattern InstanceType_C4_large :: InstanceType
pattern InstanceType_C4_large = InstanceType' "c4.large"

pattern InstanceType_C4_xlarge :: InstanceType
pattern InstanceType_C4_xlarge = InstanceType' "c4.xlarge"

pattern InstanceType_C5_12xlarge :: InstanceType
pattern InstanceType_C5_12xlarge = InstanceType' "c5.12xlarge"

pattern InstanceType_C5_18xlarge :: InstanceType
pattern InstanceType_C5_18xlarge = InstanceType' "c5.18xlarge"

pattern InstanceType_C5_24xlarge :: InstanceType
pattern InstanceType_C5_24xlarge = InstanceType' "c5.24xlarge"

pattern InstanceType_C5_2xlarge :: InstanceType
pattern InstanceType_C5_2xlarge = InstanceType' "c5.2xlarge"

pattern InstanceType_C5_4xlarge :: InstanceType
pattern InstanceType_C5_4xlarge = InstanceType' "c5.4xlarge"

pattern InstanceType_C5_9xlarge :: InstanceType
pattern InstanceType_C5_9xlarge = InstanceType' "c5.9xlarge"

pattern InstanceType_C5_large :: InstanceType
pattern InstanceType_C5_large = InstanceType' "c5.large"

pattern InstanceType_C5_metal :: InstanceType
pattern InstanceType_C5_metal = InstanceType' "c5.metal"

pattern InstanceType_C5_xlarge :: InstanceType
pattern InstanceType_C5_xlarge = InstanceType' "c5.xlarge"

pattern InstanceType_C5a_12xlarge :: InstanceType
pattern InstanceType_C5a_12xlarge = InstanceType' "c5a.12xlarge"

pattern InstanceType_C5a_16xlarge :: InstanceType
pattern InstanceType_C5a_16xlarge = InstanceType' "c5a.16xlarge"

pattern InstanceType_C5a_24xlarge :: InstanceType
pattern InstanceType_C5a_24xlarge = InstanceType' "c5a.24xlarge"

pattern InstanceType_C5a_2xlarge :: InstanceType
pattern InstanceType_C5a_2xlarge = InstanceType' "c5a.2xlarge"

pattern InstanceType_C5a_4xlarge :: InstanceType
pattern InstanceType_C5a_4xlarge = InstanceType' "c5a.4xlarge"

pattern InstanceType_C5a_8xlarge :: InstanceType
pattern InstanceType_C5a_8xlarge = InstanceType' "c5a.8xlarge"

pattern InstanceType_C5a_large :: InstanceType
pattern InstanceType_C5a_large = InstanceType' "c5a.large"

pattern InstanceType_C5a_xlarge :: InstanceType
pattern InstanceType_C5a_xlarge = InstanceType' "c5a.xlarge"

pattern InstanceType_C5ad_12xlarge :: InstanceType
pattern InstanceType_C5ad_12xlarge = InstanceType' "c5ad.12xlarge"

pattern InstanceType_C5ad_16xlarge :: InstanceType
pattern InstanceType_C5ad_16xlarge = InstanceType' "c5ad.16xlarge"

pattern InstanceType_C5ad_24xlarge :: InstanceType
pattern InstanceType_C5ad_24xlarge = InstanceType' "c5ad.24xlarge"

pattern InstanceType_C5ad_2xlarge :: InstanceType
pattern InstanceType_C5ad_2xlarge = InstanceType' "c5ad.2xlarge"

pattern InstanceType_C5ad_4xlarge :: InstanceType
pattern InstanceType_C5ad_4xlarge = InstanceType' "c5ad.4xlarge"

pattern InstanceType_C5ad_8xlarge :: InstanceType
pattern InstanceType_C5ad_8xlarge = InstanceType' "c5ad.8xlarge"

pattern InstanceType_C5ad_large :: InstanceType
pattern InstanceType_C5ad_large = InstanceType' "c5ad.large"

pattern InstanceType_C5ad_xlarge :: InstanceType
pattern InstanceType_C5ad_xlarge = InstanceType' "c5ad.xlarge"

pattern InstanceType_C5d_12xlarge :: InstanceType
pattern InstanceType_C5d_12xlarge = InstanceType' "c5d.12xlarge"

pattern InstanceType_C5d_18xlarge :: InstanceType
pattern InstanceType_C5d_18xlarge = InstanceType' "c5d.18xlarge"

pattern InstanceType_C5d_24xlarge :: InstanceType
pattern InstanceType_C5d_24xlarge = InstanceType' "c5d.24xlarge"

pattern InstanceType_C5d_2xlarge :: InstanceType
pattern InstanceType_C5d_2xlarge = InstanceType' "c5d.2xlarge"

pattern InstanceType_C5d_4xlarge :: InstanceType
pattern InstanceType_C5d_4xlarge = InstanceType' "c5d.4xlarge"

pattern InstanceType_C5d_9xlarge :: InstanceType
pattern InstanceType_C5d_9xlarge = InstanceType' "c5d.9xlarge"

pattern InstanceType_C5d_large :: InstanceType
pattern InstanceType_C5d_large = InstanceType' "c5d.large"

pattern InstanceType_C5d_metal :: InstanceType
pattern InstanceType_C5d_metal = InstanceType' "c5d.metal"

pattern InstanceType_C5d_xlarge :: InstanceType
pattern InstanceType_C5d_xlarge = InstanceType' "c5d.xlarge"

pattern InstanceType_C5n_18xlarge :: InstanceType
pattern InstanceType_C5n_18xlarge = InstanceType' "c5n.18xlarge"

pattern InstanceType_C5n_2xlarge :: InstanceType
pattern InstanceType_C5n_2xlarge = InstanceType' "c5n.2xlarge"

pattern InstanceType_C5n_4xlarge :: InstanceType
pattern InstanceType_C5n_4xlarge = InstanceType' "c5n.4xlarge"

pattern InstanceType_C5n_9xlarge :: InstanceType
pattern InstanceType_C5n_9xlarge = InstanceType' "c5n.9xlarge"

pattern InstanceType_C5n_large :: InstanceType
pattern InstanceType_C5n_large = InstanceType' "c5n.large"

pattern InstanceType_C5n_metal :: InstanceType
pattern InstanceType_C5n_metal = InstanceType' "c5n.metal"

pattern InstanceType_C5n_xlarge :: InstanceType
pattern InstanceType_C5n_xlarge = InstanceType' "c5n.xlarge"

pattern InstanceType_C6a_12xlarge :: InstanceType
pattern InstanceType_C6a_12xlarge = InstanceType' "c6a.12xlarge"

pattern InstanceType_C6a_16xlarge :: InstanceType
pattern InstanceType_C6a_16xlarge = InstanceType' "c6a.16xlarge"

pattern InstanceType_C6a_24xlarge :: InstanceType
pattern InstanceType_C6a_24xlarge = InstanceType' "c6a.24xlarge"

pattern InstanceType_C6a_2xlarge :: InstanceType
pattern InstanceType_C6a_2xlarge = InstanceType' "c6a.2xlarge"

pattern InstanceType_C6a_32xlarge :: InstanceType
pattern InstanceType_C6a_32xlarge = InstanceType' "c6a.32xlarge"

pattern InstanceType_C6a_48xlarge :: InstanceType
pattern InstanceType_C6a_48xlarge = InstanceType' "c6a.48xlarge"

pattern InstanceType_C6a_4xlarge :: InstanceType
pattern InstanceType_C6a_4xlarge = InstanceType' "c6a.4xlarge"

pattern InstanceType_C6a_8xlarge :: InstanceType
pattern InstanceType_C6a_8xlarge = InstanceType' "c6a.8xlarge"

pattern InstanceType_C6a_large :: InstanceType
pattern InstanceType_C6a_large = InstanceType' "c6a.large"

pattern InstanceType_C6a_metal :: InstanceType
pattern InstanceType_C6a_metal = InstanceType' "c6a.metal"

pattern InstanceType_C6a_xlarge :: InstanceType
pattern InstanceType_C6a_xlarge = InstanceType' "c6a.xlarge"

pattern InstanceType_C6g_12xlarge :: InstanceType
pattern InstanceType_C6g_12xlarge = InstanceType' "c6g.12xlarge"

pattern InstanceType_C6g_16xlarge :: InstanceType
pattern InstanceType_C6g_16xlarge = InstanceType' "c6g.16xlarge"

pattern InstanceType_C6g_2xlarge :: InstanceType
pattern InstanceType_C6g_2xlarge = InstanceType' "c6g.2xlarge"

pattern InstanceType_C6g_4xlarge :: InstanceType
pattern InstanceType_C6g_4xlarge = InstanceType' "c6g.4xlarge"

pattern InstanceType_C6g_8xlarge :: InstanceType
pattern InstanceType_C6g_8xlarge = InstanceType' "c6g.8xlarge"

pattern InstanceType_C6g_large :: InstanceType
pattern InstanceType_C6g_large = InstanceType' "c6g.large"

pattern InstanceType_C6g_medium :: InstanceType
pattern InstanceType_C6g_medium = InstanceType' "c6g.medium"

pattern InstanceType_C6g_metal :: InstanceType
pattern InstanceType_C6g_metal = InstanceType' "c6g.metal"

pattern InstanceType_C6g_xlarge :: InstanceType
pattern InstanceType_C6g_xlarge = InstanceType' "c6g.xlarge"

pattern InstanceType_C6gd_12xlarge :: InstanceType
pattern InstanceType_C6gd_12xlarge = InstanceType' "c6gd.12xlarge"

pattern InstanceType_C6gd_16xlarge :: InstanceType
pattern InstanceType_C6gd_16xlarge = InstanceType' "c6gd.16xlarge"

pattern InstanceType_C6gd_2xlarge :: InstanceType
pattern InstanceType_C6gd_2xlarge = InstanceType' "c6gd.2xlarge"

pattern InstanceType_C6gd_4xlarge :: InstanceType
pattern InstanceType_C6gd_4xlarge = InstanceType' "c6gd.4xlarge"

pattern InstanceType_C6gd_8xlarge :: InstanceType
pattern InstanceType_C6gd_8xlarge = InstanceType' "c6gd.8xlarge"

pattern InstanceType_C6gd_large :: InstanceType
pattern InstanceType_C6gd_large = InstanceType' "c6gd.large"

pattern InstanceType_C6gd_medium :: InstanceType
pattern InstanceType_C6gd_medium = InstanceType' "c6gd.medium"

pattern InstanceType_C6gd_metal :: InstanceType
pattern InstanceType_C6gd_metal = InstanceType' "c6gd.metal"

pattern InstanceType_C6gd_xlarge :: InstanceType
pattern InstanceType_C6gd_xlarge = InstanceType' "c6gd.xlarge"

pattern InstanceType_C6gn_12xlarge :: InstanceType
pattern InstanceType_C6gn_12xlarge = InstanceType' "c6gn.12xlarge"

pattern InstanceType_C6gn_16xlarge :: InstanceType
pattern InstanceType_C6gn_16xlarge = InstanceType' "c6gn.16xlarge"

pattern InstanceType_C6gn_2xlarge :: InstanceType
pattern InstanceType_C6gn_2xlarge = InstanceType' "c6gn.2xlarge"

pattern InstanceType_C6gn_4xlarge :: InstanceType
pattern InstanceType_C6gn_4xlarge = InstanceType' "c6gn.4xlarge"

pattern InstanceType_C6gn_8xlarge :: InstanceType
pattern InstanceType_C6gn_8xlarge = InstanceType' "c6gn.8xlarge"

pattern InstanceType_C6gn_large :: InstanceType
pattern InstanceType_C6gn_large = InstanceType' "c6gn.large"

pattern InstanceType_C6gn_medium :: InstanceType
pattern InstanceType_C6gn_medium = InstanceType' "c6gn.medium"

pattern InstanceType_C6gn_xlarge :: InstanceType
pattern InstanceType_C6gn_xlarge = InstanceType' "c6gn.xlarge"

pattern InstanceType_C6i_12xlarge :: InstanceType
pattern InstanceType_C6i_12xlarge = InstanceType' "c6i.12xlarge"

pattern InstanceType_C6i_16xlarge :: InstanceType
pattern InstanceType_C6i_16xlarge = InstanceType' "c6i.16xlarge"

pattern InstanceType_C6i_24xlarge :: InstanceType
pattern InstanceType_C6i_24xlarge = InstanceType' "c6i.24xlarge"

pattern InstanceType_C6i_2xlarge :: InstanceType
pattern InstanceType_C6i_2xlarge = InstanceType' "c6i.2xlarge"

pattern InstanceType_C6i_32xlarge :: InstanceType
pattern InstanceType_C6i_32xlarge = InstanceType' "c6i.32xlarge"

pattern InstanceType_C6i_4xlarge :: InstanceType
pattern InstanceType_C6i_4xlarge = InstanceType' "c6i.4xlarge"

pattern InstanceType_C6i_8xlarge :: InstanceType
pattern InstanceType_C6i_8xlarge = InstanceType' "c6i.8xlarge"

pattern InstanceType_C6i_large :: InstanceType
pattern InstanceType_C6i_large = InstanceType' "c6i.large"

pattern InstanceType_C6i_metal :: InstanceType
pattern InstanceType_C6i_metal = InstanceType' "c6i.metal"

pattern InstanceType_C6i_xlarge :: InstanceType
pattern InstanceType_C6i_xlarge = InstanceType' "c6i.xlarge"

pattern InstanceType_C6id_12xlarge :: InstanceType
pattern InstanceType_C6id_12xlarge = InstanceType' "c6id.12xlarge"

pattern InstanceType_C6id_16xlarge :: InstanceType
pattern InstanceType_C6id_16xlarge = InstanceType' "c6id.16xlarge"

pattern InstanceType_C6id_24xlarge :: InstanceType
pattern InstanceType_C6id_24xlarge = InstanceType' "c6id.24xlarge"

pattern InstanceType_C6id_2xlarge :: InstanceType
pattern InstanceType_C6id_2xlarge = InstanceType' "c6id.2xlarge"

pattern InstanceType_C6id_32xlarge :: InstanceType
pattern InstanceType_C6id_32xlarge = InstanceType' "c6id.32xlarge"

pattern InstanceType_C6id_4xlarge :: InstanceType
pattern InstanceType_C6id_4xlarge = InstanceType' "c6id.4xlarge"

pattern InstanceType_C6id_8xlarge :: InstanceType
pattern InstanceType_C6id_8xlarge = InstanceType' "c6id.8xlarge"

pattern InstanceType_C6id_large :: InstanceType
pattern InstanceType_C6id_large = InstanceType' "c6id.large"

pattern InstanceType_C6id_metal :: InstanceType
pattern InstanceType_C6id_metal = InstanceType' "c6id.metal"

pattern InstanceType_C6id_xlarge :: InstanceType
pattern InstanceType_C6id_xlarge = InstanceType' "c6id.xlarge"

pattern InstanceType_C7g_12xlarge :: InstanceType
pattern InstanceType_C7g_12xlarge = InstanceType' "c7g.12xlarge"

pattern InstanceType_C7g_16xlarge :: InstanceType
pattern InstanceType_C7g_16xlarge = InstanceType' "c7g.16xlarge"

pattern InstanceType_C7g_2xlarge :: InstanceType
pattern InstanceType_C7g_2xlarge = InstanceType' "c7g.2xlarge"

pattern InstanceType_C7g_4xlarge :: InstanceType
pattern InstanceType_C7g_4xlarge = InstanceType' "c7g.4xlarge"

pattern InstanceType_C7g_8xlarge :: InstanceType
pattern InstanceType_C7g_8xlarge = InstanceType' "c7g.8xlarge"

pattern InstanceType_C7g_large :: InstanceType
pattern InstanceType_C7g_large = InstanceType' "c7g.large"

pattern InstanceType_C7g_medium :: InstanceType
pattern InstanceType_C7g_medium = InstanceType' "c7g.medium"

pattern InstanceType_C7g_xlarge :: InstanceType
pattern InstanceType_C7g_xlarge = InstanceType' "c7g.xlarge"

pattern InstanceType_Cc1_4xlarge :: InstanceType
pattern InstanceType_Cc1_4xlarge = InstanceType' "cc1.4xlarge"

pattern InstanceType_Cc2_8xlarge :: InstanceType
pattern InstanceType_Cc2_8xlarge = InstanceType' "cc2.8xlarge"

pattern InstanceType_Cg1_4xlarge :: InstanceType
pattern InstanceType_Cg1_4xlarge = InstanceType' "cg1.4xlarge"

pattern InstanceType_Cr1_8xlarge :: InstanceType
pattern InstanceType_Cr1_8xlarge = InstanceType' "cr1.8xlarge"

pattern InstanceType_D2_2xlarge :: InstanceType
pattern InstanceType_D2_2xlarge = InstanceType' "d2.2xlarge"

pattern InstanceType_D2_4xlarge :: InstanceType
pattern InstanceType_D2_4xlarge = InstanceType' "d2.4xlarge"

pattern InstanceType_D2_8xlarge :: InstanceType
pattern InstanceType_D2_8xlarge = InstanceType' "d2.8xlarge"

pattern InstanceType_D2_xlarge :: InstanceType
pattern InstanceType_D2_xlarge = InstanceType' "d2.xlarge"

pattern InstanceType_D3_2xlarge :: InstanceType
pattern InstanceType_D3_2xlarge = InstanceType' "d3.2xlarge"

pattern InstanceType_D3_4xlarge :: InstanceType
pattern InstanceType_D3_4xlarge = InstanceType' "d3.4xlarge"

pattern InstanceType_D3_8xlarge :: InstanceType
pattern InstanceType_D3_8xlarge = InstanceType' "d3.8xlarge"

pattern InstanceType_D3_xlarge :: InstanceType
pattern InstanceType_D3_xlarge = InstanceType' "d3.xlarge"

pattern InstanceType_D3en_12xlarge :: InstanceType
pattern InstanceType_D3en_12xlarge = InstanceType' "d3en.12xlarge"

pattern InstanceType_D3en_2xlarge :: InstanceType
pattern InstanceType_D3en_2xlarge = InstanceType' "d3en.2xlarge"

pattern InstanceType_D3en_4xlarge :: InstanceType
pattern InstanceType_D3en_4xlarge = InstanceType' "d3en.4xlarge"

pattern InstanceType_D3en_6xlarge :: InstanceType
pattern InstanceType_D3en_6xlarge = InstanceType' "d3en.6xlarge"

pattern InstanceType_D3en_8xlarge :: InstanceType
pattern InstanceType_D3en_8xlarge = InstanceType' "d3en.8xlarge"

pattern InstanceType_D3en_xlarge :: InstanceType
pattern InstanceType_D3en_xlarge = InstanceType' "d3en.xlarge"

pattern InstanceType_Dl1_24xlarge :: InstanceType
pattern InstanceType_Dl1_24xlarge = InstanceType' "dl1.24xlarge"

pattern InstanceType_F1_16xlarge :: InstanceType
pattern InstanceType_F1_16xlarge = InstanceType' "f1.16xlarge"

pattern InstanceType_F1_2xlarge :: InstanceType
pattern InstanceType_F1_2xlarge = InstanceType' "f1.2xlarge"

pattern InstanceType_F1_4xlarge :: InstanceType
pattern InstanceType_F1_4xlarge = InstanceType' "f1.4xlarge"

pattern InstanceType_G2_2xlarge :: InstanceType
pattern InstanceType_G2_2xlarge = InstanceType' "g2.2xlarge"

pattern InstanceType_G2_8xlarge :: InstanceType
pattern InstanceType_G2_8xlarge = InstanceType' "g2.8xlarge"

pattern InstanceType_G3_16xlarge :: InstanceType
pattern InstanceType_G3_16xlarge = InstanceType' "g3.16xlarge"

pattern InstanceType_G3_4xlarge :: InstanceType
pattern InstanceType_G3_4xlarge = InstanceType' "g3.4xlarge"

pattern InstanceType_G3_8xlarge :: InstanceType
pattern InstanceType_G3_8xlarge = InstanceType' "g3.8xlarge"

pattern InstanceType_G3s_xlarge :: InstanceType
pattern InstanceType_G3s_xlarge = InstanceType' "g3s.xlarge"

pattern InstanceType_G4ad_16xlarge :: InstanceType
pattern InstanceType_G4ad_16xlarge = InstanceType' "g4ad.16xlarge"

pattern InstanceType_G4ad_2xlarge :: InstanceType
pattern InstanceType_G4ad_2xlarge = InstanceType' "g4ad.2xlarge"

pattern InstanceType_G4ad_4xlarge :: InstanceType
pattern InstanceType_G4ad_4xlarge = InstanceType' "g4ad.4xlarge"

pattern InstanceType_G4ad_8xlarge :: InstanceType
pattern InstanceType_G4ad_8xlarge = InstanceType' "g4ad.8xlarge"

pattern InstanceType_G4ad_xlarge :: InstanceType
pattern InstanceType_G4ad_xlarge = InstanceType' "g4ad.xlarge"

pattern InstanceType_G4dn_12xlarge :: InstanceType
pattern InstanceType_G4dn_12xlarge = InstanceType' "g4dn.12xlarge"

pattern InstanceType_G4dn_16xlarge :: InstanceType
pattern InstanceType_G4dn_16xlarge = InstanceType' "g4dn.16xlarge"

pattern InstanceType_G4dn_2xlarge :: InstanceType
pattern InstanceType_G4dn_2xlarge = InstanceType' "g4dn.2xlarge"

pattern InstanceType_G4dn_4xlarge :: InstanceType
pattern InstanceType_G4dn_4xlarge = InstanceType' "g4dn.4xlarge"

pattern InstanceType_G4dn_8xlarge :: InstanceType
pattern InstanceType_G4dn_8xlarge = InstanceType' "g4dn.8xlarge"

pattern InstanceType_G4dn_metal :: InstanceType
pattern InstanceType_G4dn_metal = InstanceType' "g4dn.metal"

pattern InstanceType_G4dn_xlarge :: InstanceType
pattern InstanceType_G4dn_xlarge = InstanceType' "g4dn.xlarge"

pattern InstanceType_G5_12xlarge :: InstanceType
pattern InstanceType_G5_12xlarge = InstanceType' "g5.12xlarge"

pattern InstanceType_G5_16xlarge :: InstanceType
pattern InstanceType_G5_16xlarge = InstanceType' "g5.16xlarge"

pattern InstanceType_G5_24xlarge :: InstanceType
pattern InstanceType_G5_24xlarge = InstanceType' "g5.24xlarge"

pattern InstanceType_G5_2xlarge :: InstanceType
pattern InstanceType_G5_2xlarge = InstanceType' "g5.2xlarge"

pattern InstanceType_G5_48xlarge :: InstanceType
pattern InstanceType_G5_48xlarge = InstanceType' "g5.48xlarge"

pattern InstanceType_G5_4xlarge :: InstanceType
pattern InstanceType_G5_4xlarge = InstanceType' "g5.4xlarge"

pattern InstanceType_G5_8xlarge :: InstanceType
pattern InstanceType_G5_8xlarge = InstanceType' "g5.8xlarge"

pattern InstanceType_G5_xlarge :: InstanceType
pattern InstanceType_G5_xlarge = InstanceType' "g5.xlarge"

pattern InstanceType_G5g_16xlarge :: InstanceType
pattern InstanceType_G5g_16xlarge = InstanceType' "g5g.16xlarge"

pattern InstanceType_G5g_2xlarge :: InstanceType
pattern InstanceType_G5g_2xlarge = InstanceType' "g5g.2xlarge"

pattern InstanceType_G5g_4xlarge :: InstanceType
pattern InstanceType_G5g_4xlarge = InstanceType' "g5g.4xlarge"

pattern InstanceType_G5g_8xlarge :: InstanceType
pattern InstanceType_G5g_8xlarge = InstanceType' "g5g.8xlarge"

pattern InstanceType_G5g_metal :: InstanceType
pattern InstanceType_G5g_metal = InstanceType' "g5g.metal"

pattern InstanceType_G5g_xlarge :: InstanceType
pattern InstanceType_G5g_xlarge = InstanceType' "g5g.xlarge"

pattern InstanceType_H1_16xlarge :: InstanceType
pattern InstanceType_H1_16xlarge = InstanceType' "h1.16xlarge"

pattern InstanceType_H1_2xlarge :: InstanceType
pattern InstanceType_H1_2xlarge = InstanceType' "h1.2xlarge"

pattern InstanceType_H1_4xlarge :: InstanceType
pattern InstanceType_H1_4xlarge = InstanceType' "h1.4xlarge"

pattern InstanceType_H1_8xlarge :: InstanceType
pattern InstanceType_H1_8xlarge = InstanceType' "h1.8xlarge"

pattern InstanceType_Hi1_4xlarge :: InstanceType
pattern InstanceType_Hi1_4xlarge = InstanceType' "hi1.4xlarge"

pattern InstanceType_Hpc6a_48xlarge :: InstanceType
pattern InstanceType_Hpc6a_48xlarge = InstanceType' "hpc6a.48xlarge"

pattern InstanceType_Hpc6id_32xlarge :: InstanceType
pattern InstanceType_Hpc6id_32xlarge = InstanceType' "hpc6id.32xlarge"

pattern InstanceType_Hs1_8xlarge :: InstanceType
pattern InstanceType_Hs1_8xlarge = InstanceType' "hs1.8xlarge"

pattern InstanceType_I2_2xlarge :: InstanceType
pattern InstanceType_I2_2xlarge = InstanceType' "i2.2xlarge"

pattern InstanceType_I2_4xlarge :: InstanceType
pattern InstanceType_I2_4xlarge = InstanceType' "i2.4xlarge"

pattern InstanceType_I2_8xlarge :: InstanceType
pattern InstanceType_I2_8xlarge = InstanceType' "i2.8xlarge"

pattern InstanceType_I2_xlarge :: InstanceType
pattern InstanceType_I2_xlarge = InstanceType' "i2.xlarge"

pattern InstanceType_I3_16xlarge :: InstanceType
pattern InstanceType_I3_16xlarge = InstanceType' "i3.16xlarge"

pattern InstanceType_I3_2xlarge :: InstanceType
pattern InstanceType_I3_2xlarge = InstanceType' "i3.2xlarge"

pattern InstanceType_I3_4xlarge :: InstanceType
pattern InstanceType_I3_4xlarge = InstanceType' "i3.4xlarge"

pattern InstanceType_I3_8xlarge :: InstanceType
pattern InstanceType_I3_8xlarge = InstanceType' "i3.8xlarge"

pattern InstanceType_I3_large :: InstanceType
pattern InstanceType_I3_large = InstanceType' "i3.large"

pattern InstanceType_I3_metal :: InstanceType
pattern InstanceType_I3_metal = InstanceType' "i3.metal"

pattern InstanceType_I3_xlarge :: InstanceType
pattern InstanceType_I3_xlarge = InstanceType' "i3.xlarge"

pattern InstanceType_I3en_12xlarge :: InstanceType
pattern InstanceType_I3en_12xlarge = InstanceType' "i3en.12xlarge"

pattern InstanceType_I3en_24xlarge :: InstanceType
pattern InstanceType_I3en_24xlarge = InstanceType' "i3en.24xlarge"

pattern InstanceType_I3en_2xlarge :: InstanceType
pattern InstanceType_I3en_2xlarge = InstanceType' "i3en.2xlarge"

pattern InstanceType_I3en_3xlarge :: InstanceType
pattern InstanceType_I3en_3xlarge = InstanceType' "i3en.3xlarge"

pattern InstanceType_I3en_6xlarge :: InstanceType
pattern InstanceType_I3en_6xlarge = InstanceType' "i3en.6xlarge"

pattern InstanceType_I3en_large :: InstanceType
pattern InstanceType_I3en_large = InstanceType' "i3en.large"

pattern InstanceType_I3en_metal :: InstanceType
pattern InstanceType_I3en_metal = InstanceType' "i3en.metal"

pattern InstanceType_I3en_xlarge :: InstanceType
pattern InstanceType_I3en_xlarge = InstanceType' "i3en.xlarge"

pattern InstanceType_I4i_16xlarge :: InstanceType
pattern InstanceType_I4i_16xlarge = InstanceType' "i4i.16xlarge"

pattern InstanceType_I4i_2xlarge :: InstanceType
pattern InstanceType_I4i_2xlarge = InstanceType' "i4i.2xlarge"

pattern InstanceType_I4i_32xlarge :: InstanceType
pattern InstanceType_I4i_32xlarge = InstanceType' "i4i.32xlarge"

pattern InstanceType_I4i_4xlarge :: InstanceType
pattern InstanceType_I4i_4xlarge = InstanceType' "i4i.4xlarge"

pattern InstanceType_I4i_8xlarge :: InstanceType
pattern InstanceType_I4i_8xlarge = InstanceType' "i4i.8xlarge"

pattern InstanceType_I4i_large :: InstanceType
pattern InstanceType_I4i_large = InstanceType' "i4i.large"

pattern InstanceType_I4i_metal :: InstanceType
pattern InstanceType_I4i_metal = InstanceType' "i4i.metal"

pattern InstanceType_I4i_xlarge :: InstanceType
pattern InstanceType_I4i_xlarge = InstanceType' "i4i.xlarge"

pattern InstanceType_Im4gn_16xlarge :: InstanceType
pattern InstanceType_Im4gn_16xlarge = InstanceType' "im4gn.16xlarge"

pattern InstanceType_Im4gn_2xlarge :: InstanceType
pattern InstanceType_Im4gn_2xlarge = InstanceType' "im4gn.2xlarge"

pattern InstanceType_Im4gn_4xlarge :: InstanceType
pattern InstanceType_Im4gn_4xlarge = InstanceType' "im4gn.4xlarge"

pattern InstanceType_Im4gn_8xlarge :: InstanceType
pattern InstanceType_Im4gn_8xlarge = InstanceType' "im4gn.8xlarge"

pattern InstanceType_Im4gn_large :: InstanceType
pattern InstanceType_Im4gn_large = InstanceType' "im4gn.large"

pattern InstanceType_Im4gn_xlarge :: InstanceType
pattern InstanceType_Im4gn_xlarge = InstanceType' "im4gn.xlarge"

pattern InstanceType_Inf1_24xlarge :: InstanceType
pattern InstanceType_Inf1_24xlarge = InstanceType' "inf1.24xlarge"

pattern InstanceType_Inf1_2xlarge :: InstanceType
pattern InstanceType_Inf1_2xlarge = InstanceType' "inf1.2xlarge"

pattern InstanceType_Inf1_6xlarge :: InstanceType
pattern InstanceType_Inf1_6xlarge = InstanceType' "inf1.6xlarge"

pattern InstanceType_Inf1_xlarge :: InstanceType
pattern InstanceType_Inf1_xlarge = InstanceType' "inf1.xlarge"

pattern InstanceType_Is4gen_2xlarge :: InstanceType
pattern InstanceType_Is4gen_2xlarge = InstanceType' "is4gen.2xlarge"

pattern InstanceType_Is4gen_4xlarge :: InstanceType
pattern InstanceType_Is4gen_4xlarge = InstanceType' "is4gen.4xlarge"

pattern InstanceType_Is4gen_8xlarge :: InstanceType
pattern InstanceType_Is4gen_8xlarge = InstanceType' "is4gen.8xlarge"

pattern InstanceType_Is4gen_large :: InstanceType
pattern InstanceType_Is4gen_large = InstanceType' "is4gen.large"

pattern InstanceType_Is4gen_medium :: InstanceType
pattern InstanceType_Is4gen_medium = InstanceType' "is4gen.medium"

pattern InstanceType_Is4gen_xlarge :: InstanceType
pattern InstanceType_Is4gen_xlarge = InstanceType' "is4gen.xlarge"

pattern InstanceType_M1_large :: InstanceType
pattern InstanceType_M1_large = InstanceType' "m1.large"

pattern InstanceType_M1_medium :: InstanceType
pattern InstanceType_M1_medium = InstanceType' "m1.medium"

pattern InstanceType_M1_small :: InstanceType
pattern InstanceType_M1_small = InstanceType' "m1.small"

pattern InstanceType_M1_xlarge :: InstanceType
pattern InstanceType_M1_xlarge = InstanceType' "m1.xlarge"

pattern InstanceType_M2_2xlarge :: InstanceType
pattern InstanceType_M2_2xlarge = InstanceType' "m2.2xlarge"

pattern InstanceType_M2_4xlarge :: InstanceType
pattern InstanceType_M2_4xlarge = InstanceType' "m2.4xlarge"

pattern InstanceType_M2_xlarge :: InstanceType
pattern InstanceType_M2_xlarge = InstanceType' "m2.xlarge"

pattern InstanceType_M3_2xlarge :: InstanceType
pattern InstanceType_M3_2xlarge = InstanceType' "m3.2xlarge"

pattern InstanceType_M3_large :: InstanceType
pattern InstanceType_M3_large = InstanceType' "m3.large"

pattern InstanceType_M3_medium :: InstanceType
pattern InstanceType_M3_medium = InstanceType' "m3.medium"

pattern InstanceType_M3_xlarge :: InstanceType
pattern InstanceType_M3_xlarge = InstanceType' "m3.xlarge"

pattern InstanceType_M4_10xlarge :: InstanceType
pattern InstanceType_M4_10xlarge = InstanceType' "m4.10xlarge"

pattern InstanceType_M4_16xlarge :: InstanceType
pattern InstanceType_M4_16xlarge = InstanceType' "m4.16xlarge"

pattern InstanceType_M4_2xlarge :: InstanceType
pattern InstanceType_M4_2xlarge = InstanceType' "m4.2xlarge"

pattern InstanceType_M4_4xlarge :: InstanceType
pattern InstanceType_M4_4xlarge = InstanceType' "m4.4xlarge"

pattern InstanceType_M4_large :: InstanceType
pattern InstanceType_M4_large = InstanceType' "m4.large"

pattern InstanceType_M4_xlarge :: InstanceType
pattern InstanceType_M4_xlarge = InstanceType' "m4.xlarge"

pattern InstanceType_M5_12xlarge :: InstanceType
pattern InstanceType_M5_12xlarge = InstanceType' "m5.12xlarge"

pattern InstanceType_M5_16xlarge :: InstanceType
pattern InstanceType_M5_16xlarge = InstanceType' "m5.16xlarge"

pattern InstanceType_M5_24xlarge :: InstanceType
pattern InstanceType_M5_24xlarge = InstanceType' "m5.24xlarge"

pattern InstanceType_M5_2xlarge :: InstanceType
pattern InstanceType_M5_2xlarge = InstanceType' "m5.2xlarge"

pattern InstanceType_M5_4xlarge :: InstanceType
pattern InstanceType_M5_4xlarge = InstanceType' "m5.4xlarge"

pattern InstanceType_M5_8xlarge :: InstanceType
pattern InstanceType_M5_8xlarge = InstanceType' "m5.8xlarge"

pattern InstanceType_M5_large :: InstanceType
pattern InstanceType_M5_large = InstanceType' "m5.large"

pattern InstanceType_M5_metal :: InstanceType
pattern InstanceType_M5_metal = InstanceType' "m5.metal"

pattern InstanceType_M5_xlarge :: InstanceType
pattern InstanceType_M5_xlarge = InstanceType' "m5.xlarge"

pattern InstanceType_M5a_12xlarge :: InstanceType
pattern InstanceType_M5a_12xlarge = InstanceType' "m5a.12xlarge"

pattern InstanceType_M5a_16xlarge :: InstanceType
pattern InstanceType_M5a_16xlarge = InstanceType' "m5a.16xlarge"

pattern InstanceType_M5a_24xlarge :: InstanceType
pattern InstanceType_M5a_24xlarge = InstanceType' "m5a.24xlarge"

pattern InstanceType_M5a_2xlarge :: InstanceType
pattern InstanceType_M5a_2xlarge = InstanceType' "m5a.2xlarge"

pattern InstanceType_M5a_4xlarge :: InstanceType
pattern InstanceType_M5a_4xlarge = InstanceType' "m5a.4xlarge"

pattern InstanceType_M5a_8xlarge :: InstanceType
pattern InstanceType_M5a_8xlarge = InstanceType' "m5a.8xlarge"

pattern InstanceType_M5a_large :: InstanceType
pattern InstanceType_M5a_large = InstanceType' "m5a.large"

pattern InstanceType_M5a_xlarge :: InstanceType
pattern InstanceType_M5a_xlarge = InstanceType' "m5a.xlarge"

pattern InstanceType_M5ad_12xlarge :: InstanceType
pattern InstanceType_M5ad_12xlarge = InstanceType' "m5ad.12xlarge"

pattern InstanceType_M5ad_16xlarge :: InstanceType
pattern InstanceType_M5ad_16xlarge = InstanceType' "m5ad.16xlarge"

pattern InstanceType_M5ad_24xlarge :: InstanceType
pattern InstanceType_M5ad_24xlarge = InstanceType' "m5ad.24xlarge"

pattern InstanceType_M5ad_2xlarge :: InstanceType
pattern InstanceType_M5ad_2xlarge = InstanceType' "m5ad.2xlarge"

pattern InstanceType_M5ad_4xlarge :: InstanceType
pattern InstanceType_M5ad_4xlarge = InstanceType' "m5ad.4xlarge"

pattern InstanceType_M5ad_8xlarge :: InstanceType
pattern InstanceType_M5ad_8xlarge = InstanceType' "m5ad.8xlarge"

pattern InstanceType_M5ad_large :: InstanceType
pattern InstanceType_M5ad_large = InstanceType' "m5ad.large"

pattern InstanceType_M5ad_xlarge :: InstanceType
pattern InstanceType_M5ad_xlarge = InstanceType' "m5ad.xlarge"

pattern InstanceType_M5d_12xlarge :: InstanceType
pattern InstanceType_M5d_12xlarge = InstanceType' "m5d.12xlarge"

pattern InstanceType_M5d_16xlarge :: InstanceType
pattern InstanceType_M5d_16xlarge = InstanceType' "m5d.16xlarge"

pattern InstanceType_M5d_24xlarge :: InstanceType
pattern InstanceType_M5d_24xlarge = InstanceType' "m5d.24xlarge"

pattern InstanceType_M5d_2xlarge :: InstanceType
pattern InstanceType_M5d_2xlarge = InstanceType' "m5d.2xlarge"

pattern InstanceType_M5d_4xlarge :: InstanceType
pattern InstanceType_M5d_4xlarge = InstanceType' "m5d.4xlarge"

pattern InstanceType_M5d_8xlarge :: InstanceType
pattern InstanceType_M5d_8xlarge = InstanceType' "m5d.8xlarge"

pattern InstanceType_M5d_large :: InstanceType
pattern InstanceType_M5d_large = InstanceType' "m5d.large"

pattern InstanceType_M5d_metal :: InstanceType
pattern InstanceType_M5d_metal = InstanceType' "m5d.metal"

pattern InstanceType_M5d_xlarge :: InstanceType
pattern InstanceType_M5d_xlarge = InstanceType' "m5d.xlarge"

pattern InstanceType_M5dn_12xlarge :: InstanceType
pattern InstanceType_M5dn_12xlarge = InstanceType' "m5dn.12xlarge"

pattern InstanceType_M5dn_16xlarge :: InstanceType
pattern InstanceType_M5dn_16xlarge = InstanceType' "m5dn.16xlarge"

pattern InstanceType_M5dn_24xlarge :: InstanceType
pattern InstanceType_M5dn_24xlarge = InstanceType' "m5dn.24xlarge"

pattern InstanceType_M5dn_2xlarge :: InstanceType
pattern InstanceType_M5dn_2xlarge = InstanceType' "m5dn.2xlarge"

pattern InstanceType_M5dn_4xlarge :: InstanceType
pattern InstanceType_M5dn_4xlarge = InstanceType' "m5dn.4xlarge"

pattern InstanceType_M5dn_8xlarge :: InstanceType
pattern InstanceType_M5dn_8xlarge = InstanceType' "m5dn.8xlarge"

pattern InstanceType_M5dn_large :: InstanceType
pattern InstanceType_M5dn_large = InstanceType' "m5dn.large"

pattern InstanceType_M5dn_metal :: InstanceType
pattern InstanceType_M5dn_metal = InstanceType' "m5dn.metal"

pattern InstanceType_M5dn_xlarge :: InstanceType
pattern InstanceType_M5dn_xlarge = InstanceType' "m5dn.xlarge"

pattern InstanceType_M5n_12xlarge :: InstanceType
pattern InstanceType_M5n_12xlarge = InstanceType' "m5n.12xlarge"

pattern InstanceType_M5n_16xlarge :: InstanceType
pattern InstanceType_M5n_16xlarge = InstanceType' "m5n.16xlarge"

pattern InstanceType_M5n_24xlarge :: InstanceType
pattern InstanceType_M5n_24xlarge = InstanceType' "m5n.24xlarge"

pattern InstanceType_M5n_2xlarge :: InstanceType
pattern InstanceType_M5n_2xlarge = InstanceType' "m5n.2xlarge"

pattern InstanceType_M5n_4xlarge :: InstanceType
pattern InstanceType_M5n_4xlarge = InstanceType' "m5n.4xlarge"

pattern InstanceType_M5n_8xlarge :: InstanceType
pattern InstanceType_M5n_8xlarge = InstanceType' "m5n.8xlarge"

pattern InstanceType_M5n_large :: InstanceType
pattern InstanceType_M5n_large = InstanceType' "m5n.large"

pattern InstanceType_M5n_metal :: InstanceType
pattern InstanceType_M5n_metal = InstanceType' "m5n.metal"

pattern InstanceType_M5n_xlarge :: InstanceType
pattern InstanceType_M5n_xlarge = InstanceType' "m5n.xlarge"

pattern InstanceType_M5zn_12xlarge :: InstanceType
pattern InstanceType_M5zn_12xlarge = InstanceType' "m5zn.12xlarge"

pattern InstanceType_M5zn_2xlarge :: InstanceType
pattern InstanceType_M5zn_2xlarge = InstanceType' "m5zn.2xlarge"

pattern InstanceType_M5zn_3xlarge :: InstanceType
pattern InstanceType_M5zn_3xlarge = InstanceType' "m5zn.3xlarge"

pattern InstanceType_M5zn_6xlarge :: InstanceType
pattern InstanceType_M5zn_6xlarge = InstanceType' "m5zn.6xlarge"

pattern InstanceType_M5zn_large :: InstanceType
pattern InstanceType_M5zn_large = InstanceType' "m5zn.large"

pattern InstanceType_M5zn_metal :: InstanceType
pattern InstanceType_M5zn_metal = InstanceType' "m5zn.metal"

pattern InstanceType_M5zn_xlarge :: InstanceType
pattern InstanceType_M5zn_xlarge = InstanceType' "m5zn.xlarge"

pattern InstanceType_M6a_12xlarge :: InstanceType
pattern InstanceType_M6a_12xlarge = InstanceType' "m6a.12xlarge"

pattern InstanceType_M6a_16xlarge :: InstanceType
pattern InstanceType_M6a_16xlarge = InstanceType' "m6a.16xlarge"

pattern InstanceType_M6a_24xlarge :: InstanceType
pattern InstanceType_M6a_24xlarge = InstanceType' "m6a.24xlarge"

pattern InstanceType_M6a_2xlarge :: InstanceType
pattern InstanceType_M6a_2xlarge = InstanceType' "m6a.2xlarge"

pattern InstanceType_M6a_32xlarge :: InstanceType
pattern InstanceType_M6a_32xlarge = InstanceType' "m6a.32xlarge"

pattern InstanceType_M6a_48xlarge :: InstanceType
pattern InstanceType_M6a_48xlarge = InstanceType' "m6a.48xlarge"

pattern InstanceType_M6a_4xlarge :: InstanceType
pattern InstanceType_M6a_4xlarge = InstanceType' "m6a.4xlarge"

pattern InstanceType_M6a_8xlarge :: InstanceType
pattern InstanceType_M6a_8xlarge = InstanceType' "m6a.8xlarge"

pattern InstanceType_M6a_large :: InstanceType
pattern InstanceType_M6a_large = InstanceType' "m6a.large"

pattern InstanceType_M6a_metal :: InstanceType
pattern InstanceType_M6a_metal = InstanceType' "m6a.metal"

pattern InstanceType_M6a_xlarge :: InstanceType
pattern InstanceType_M6a_xlarge = InstanceType' "m6a.xlarge"

pattern InstanceType_M6g_12xlarge :: InstanceType
pattern InstanceType_M6g_12xlarge = InstanceType' "m6g.12xlarge"

pattern InstanceType_M6g_16xlarge :: InstanceType
pattern InstanceType_M6g_16xlarge = InstanceType' "m6g.16xlarge"

pattern InstanceType_M6g_2xlarge :: InstanceType
pattern InstanceType_M6g_2xlarge = InstanceType' "m6g.2xlarge"

pattern InstanceType_M6g_4xlarge :: InstanceType
pattern InstanceType_M6g_4xlarge = InstanceType' "m6g.4xlarge"

pattern InstanceType_M6g_8xlarge :: InstanceType
pattern InstanceType_M6g_8xlarge = InstanceType' "m6g.8xlarge"

pattern InstanceType_M6g_large :: InstanceType
pattern InstanceType_M6g_large = InstanceType' "m6g.large"

pattern InstanceType_M6g_medium :: InstanceType
pattern InstanceType_M6g_medium = InstanceType' "m6g.medium"

pattern InstanceType_M6g_metal :: InstanceType
pattern InstanceType_M6g_metal = InstanceType' "m6g.metal"

pattern InstanceType_M6g_xlarge :: InstanceType
pattern InstanceType_M6g_xlarge = InstanceType' "m6g.xlarge"

pattern InstanceType_M6gd_12xlarge :: InstanceType
pattern InstanceType_M6gd_12xlarge = InstanceType' "m6gd.12xlarge"

pattern InstanceType_M6gd_16xlarge :: InstanceType
pattern InstanceType_M6gd_16xlarge = InstanceType' "m6gd.16xlarge"

pattern InstanceType_M6gd_2xlarge :: InstanceType
pattern InstanceType_M6gd_2xlarge = InstanceType' "m6gd.2xlarge"

pattern InstanceType_M6gd_4xlarge :: InstanceType
pattern InstanceType_M6gd_4xlarge = InstanceType' "m6gd.4xlarge"

pattern InstanceType_M6gd_8xlarge :: InstanceType
pattern InstanceType_M6gd_8xlarge = InstanceType' "m6gd.8xlarge"

pattern InstanceType_M6gd_large :: InstanceType
pattern InstanceType_M6gd_large = InstanceType' "m6gd.large"

pattern InstanceType_M6gd_medium :: InstanceType
pattern InstanceType_M6gd_medium = InstanceType' "m6gd.medium"

pattern InstanceType_M6gd_metal :: InstanceType
pattern InstanceType_M6gd_metal = InstanceType' "m6gd.metal"

pattern InstanceType_M6gd_xlarge :: InstanceType
pattern InstanceType_M6gd_xlarge = InstanceType' "m6gd.xlarge"

pattern InstanceType_M6i_12xlarge :: InstanceType
pattern InstanceType_M6i_12xlarge = InstanceType' "m6i.12xlarge"

pattern InstanceType_M6i_16xlarge :: InstanceType
pattern InstanceType_M6i_16xlarge = InstanceType' "m6i.16xlarge"

pattern InstanceType_M6i_24xlarge :: InstanceType
pattern InstanceType_M6i_24xlarge = InstanceType' "m6i.24xlarge"

pattern InstanceType_M6i_2xlarge :: InstanceType
pattern InstanceType_M6i_2xlarge = InstanceType' "m6i.2xlarge"

pattern InstanceType_M6i_32xlarge :: InstanceType
pattern InstanceType_M6i_32xlarge = InstanceType' "m6i.32xlarge"

pattern InstanceType_M6i_4xlarge :: InstanceType
pattern InstanceType_M6i_4xlarge = InstanceType' "m6i.4xlarge"

pattern InstanceType_M6i_8xlarge :: InstanceType
pattern InstanceType_M6i_8xlarge = InstanceType' "m6i.8xlarge"

pattern InstanceType_M6i_large :: InstanceType
pattern InstanceType_M6i_large = InstanceType' "m6i.large"

pattern InstanceType_M6i_metal :: InstanceType
pattern InstanceType_M6i_metal = InstanceType' "m6i.metal"

pattern InstanceType_M6i_xlarge :: InstanceType
pattern InstanceType_M6i_xlarge = InstanceType' "m6i.xlarge"

pattern InstanceType_M6id_12xlarge :: InstanceType
pattern InstanceType_M6id_12xlarge = InstanceType' "m6id.12xlarge"

pattern InstanceType_M6id_16xlarge :: InstanceType
pattern InstanceType_M6id_16xlarge = InstanceType' "m6id.16xlarge"

pattern InstanceType_M6id_24xlarge :: InstanceType
pattern InstanceType_M6id_24xlarge = InstanceType' "m6id.24xlarge"

pattern InstanceType_M6id_2xlarge :: InstanceType
pattern InstanceType_M6id_2xlarge = InstanceType' "m6id.2xlarge"

pattern InstanceType_M6id_32xlarge :: InstanceType
pattern InstanceType_M6id_32xlarge = InstanceType' "m6id.32xlarge"

pattern InstanceType_M6id_4xlarge :: InstanceType
pattern InstanceType_M6id_4xlarge = InstanceType' "m6id.4xlarge"

pattern InstanceType_M6id_8xlarge :: InstanceType
pattern InstanceType_M6id_8xlarge = InstanceType' "m6id.8xlarge"

pattern InstanceType_M6id_large :: InstanceType
pattern InstanceType_M6id_large = InstanceType' "m6id.large"

pattern InstanceType_M6id_metal :: InstanceType
pattern InstanceType_M6id_metal = InstanceType' "m6id.metal"

pattern InstanceType_M6id_xlarge :: InstanceType
pattern InstanceType_M6id_xlarge = InstanceType' "m6id.xlarge"

pattern InstanceType_Mac1_metal :: InstanceType
pattern InstanceType_Mac1_metal = InstanceType' "mac1.metal"

pattern InstanceType_Mac2_metal :: InstanceType
pattern InstanceType_Mac2_metal = InstanceType' "mac2.metal"

pattern InstanceType_P2_16xlarge :: InstanceType
pattern InstanceType_P2_16xlarge = InstanceType' "p2.16xlarge"

pattern InstanceType_P2_8xlarge :: InstanceType
pattern InstanceType_P2_8xlarge = InstanceType' "p2.8xlarge"

pattern InstanceType_P2_xlarge :: InstanceType
pattern InstanceType_P2_xlarge = InstanceType' "p2.xlarge"

pattern InstanceType_P3_16xlarge :: InstanceType
pattern InstanceType_P3_16xlarge = InstanceType' "p3.16xlarge"

pattern InstanceType_P3_2xlarge :: InstanceType
pattern InstanceType_P3_2xlarge = InstanceType' "p3.2xlarge"

pattern InstanceType_P3_8xlarge :: InstanceType
pattern InstanceType_P3_8xlarge = InstanceType' "p3.8xlarge"

pattern InstanceType_P3dn_24xlarge :: InstanceType
pattern InstanceType_P3dn_24xlarge = InstanceType' "p3dn.24xlarge"

pattern InstanceType_P4d_24xlarge :: InstanceType
pattern InstanceType_P4d_24xlarge = InstanceType' "p4d.24xlarge"

pattern InstanceType_P4de_24xlarge :: InstanceType
pattern InstanceType_P4de_24xlarge = InstanceType' "p4de.24xlarge"

pattern InstanceType_R3_2xlarge :: InstanceType
pattern InstanceType_R3_2xlarge = InstanceType' "r3.2xlarge"

pattern InstanceType_R3_4xlarge :: InstanceType
pattern InstanceType_R3_4xlarge = InstanceType' "r3.4xlarge"

pattern InstanceType_R3_8xlarge :: InstanceType
pattern InstanceType_R3_8xlarge = InstanceType' "r3.8xlarge"

pattern InstanceType_R3_large :: InstanceType
pattern InstanceType_R3_large = InstanceType' "r3.large"

pattern InstanceType_R3_xlarge :: InstanceType
pattern InstanceType_R3_xlarge = InstanceType' "r3.xlarge"

pattern InstanceType_R4_16xlarge :: InstanceType
pattern InstanceType_R4_16xlarge = InstanceType' "r4.16xlarge"

pattern InstanceType_R4_2xlarge :: InstanceType
pattern InstanceType_R4_2xlarge = InstanceType' "r4.2xlarge"

pattern InstanceType_R4_4xlarge :: InstanceType
pattern InstanceType_R4_4xlarge = InstanceType' "r4.4xlarge"

pattern InstanceType_R4_8xlarge :: InstanceType
pattern InstanceType_R4_8xlarge = InstanceType' "r4.8xlarge"

pattern InstanceType_R4_large :: InstanceType
pattern InstanceType_R4_large = InstanceType' "r4.large"

pattern InstanceType_R4_xlarge :: InstanceType
pattern InstanceType_R4_xlarge = InstanceType' "r4.xlarge"

pattern InstanceType_R5_12xlarge :: InstanceType
pattern InstanceType_R5_12xlarge = InstanceType' "r5.12xlarge"

pattern InstanceType_R5_16xlarge :: InstanceType
pattern InstanceType_R5_16xlarge = InstanceType' "r5.16xlarge"

pattern InstanceType_R5_24xlarge :: InstanceType
pattern InstanceType_R5_24xlarge = InstanceType' "r5.24xlarge"

pattern InstanceType_R5_2xlarge :: InstanceType
pattern InstanceType_R5_2xlarge = InstanceType' "r5.2xlarge"

pattern InstanceType_R5_4xlarge :: InstanceType
pattern InstanceType_R5_4xlarge = InstanceType' "r5.4xlarge"

pattern InstanceType_R5_8xlarge :: InstanceType
pattern InstanceType_R5_8xlarge = InstanceType' "r5.8xlarge"

pattern InstanceType_R5_large :: InstanceType
pattern InstanceType_R5_large = InstanceType' "r5.large"

pattern InstanceType_R5_metal :: InstanceType
pattern InstanceType_R5_metal = InstanceType' "r5.metal"

pattern InstanceType_R5_xlarge :: InstanceType
pattern InstanceType_R5_xlarge = InstanceType' "r5.xlarge"

pattern InstanceType_R5a_12xlarge :: InstanceType
pattern InstanceType_R5a_12xlarge = InstanceType' "r5a.12xlarge"

pattern InstanceType_R5a_16xlarge :: InstanceType
pattern InstanceType_R5a_16xlarge = InstanceType' "r5a.16xlarge"

pattern InstanceType_R5a_24xlarge :: InstanceType
pattern InstanceType_R5a_24xlarge = InstanceType' "r5a.24xlarge"

pattern InstanceType_R5a_2xlarge :: InstanceType
pattern InstanceType_R5a_2xlarge = InstanceType' "r5a.2xlarge"

pattern InstanceType_R5a_4xlarge :: InstanceType
pattern InstanceType_R5a_4xlarge = InstanceType' "r5a.4xlarge"

pattern InstanceType_R5a_8xlarge :: InstanceType
pattern InstanceType_R5a_8xlarge = InstanceType' "r5a.8xlarge"

pattern InstanceType_R5a_large :: InstanceType
pattern InstanceType_R5a_large = InstanceType' "r5a.large"

pattern InstanceType_R5a_xlarge :: InstanceType
pattern InstanceType_R5a_xlarge = InstanceType' "r5a.xlarge"

pattern InstanceType_R5ad_12xlarge :: InstanceType
pattern InstanceType_R5ad_12xlarge = InstanceType' "r5ad.12xlarge"

pattern InstanceType_R5ad_16xlarge :: InstanceType
pattern InstanceType_R5ad_16xlarge = InstanceType' "r5ad.16xlarge"

pattern InstanceType_R5ad_24xlarge :: InstanceType
pattern InstanceType_R5ad_24xlarge = InstanceType' "r5ad.24xlarge"

pattern InstanceType_R5ad_2xlarge :: InstanceType
pattern InstanceType_R5ad_2xlarge = InstanceType' "r5ad.2xlarge"

pattern InstanceType_R5ad_4xlarge :: InstanceType
pattern InstanceType_R5ad_4xlarge = InstanceType' "r5ad.4xlarge"

pattern InstanceType_R5ad_8xlarge :: InstanceType
pattern InstanceType_R5ad_8xlarge = InstanceType' "r5ad.8xlarge"

pattern InstanceType_R5ad_large :: InstanceType
pattern InstanceType_R5ad_large = InstanceType' "r5ad.large"

pattern InstanceType_R5ad_xlarge :: InstanceType
pattern InstanceType_R5ad_xlarge = InstanceType' "r5ad.xlarge"

pattern InstanceType_R5b_12xlarge :: InstanceType
pattern InstanceType_R5b_12xlarge = InstanceType' "r5b.12xlarge"

pattern InstanceType_R5b_16xlarge :: InstanceType
pattern InstanceType_R5b_16xlarge = InstanceType' "r5b.16xlarge"

pattern InstanceType_R5b_24xlarge :: InstanceType
pattern InstanceType_R5b_24xlarge = InstanceType' "r5b.24xlarge"

pattern InstanceType_R5b_2xlarge :: InstanceType
pattern InstanceType_R5b_2xlarge = InstanceType' "r5b.2xlarge"

pattern InstanceType_R5b_4xlarge :: InstanceType
pattern InstanceType_R5b_4xlarge = InstanceType' "r5b.4xlarge"

pattern InstanceType_R5b_8xlarge :: InstanceType
pattern InstanceType_R5b_8xlarge = InstanceType' "r5b.8xlarge"

pattern InstanceType_R5b_large :: InstanceType
pattern InstanceType_R5b_large = InstanceType' "r5b.large"

pattern InstanceType_R5b_metal :: InstanceType
pattern InstanceType_R5b_metal = InstanceType' "r5b.metal"

pattern InstanceType_R5b_xlarge :: InstanceType
pattern InstanceType_R5b_xlarge = InstanceType' "r5b.xlarge"

pattern InstanceType_R5d_12xlarge :: InstanceType
pattern InstanceType_R5d_12xlarge = InstanceType' "r5d.12xlarge"

pattern InstanceType_R5d_16xlarge :: InstanceType
pattern InstanceType_R5d_16xlarge = InstanceType' "r5d.16xlarge"

pattern InstanceType_R5d_24xlarge :: InstanceType
pattern InstanceType_R5d_24xlarge = InstanceType' "r5d.24xlarge"

pattern InstanceType_R5d_2xlarge :: InstanceType
pattern InstanceType_R5d_2xlarge = InstanceType' "r5d.2xlarge"

pattern InstanceType_R5d_4xlarge :: InstanceType
pattern InstanceType_R5d_4xlarge = InstanceType' "r5d.4xlarge"

pattern InstanceType_R5d_8xlarge :: InstanceType
pattern InstanceType_R5d_8xlarge = InstanceType' "r5d.8xlarge"

pattern InstanceType_R5d_large :: InstanceType
pattern InstanceType_R5d_large = InstanceType' "r5d.large"

pattern InstanceType_R5d_metal :: InstanceType
pattern InstanceType_R5d_metal = InstanceType' "r5d.metal"

pattern InstanceType_R5d_xlarge :: InstanceType
pattern InstanceType_R5d_xlarge = InstanceType' "r5d.xlarge"

pattern InstanceType_R5dn_12xlarge :: InstanceType
pattern InstanceType_R5dn_12xlarge = InstanceType' "r5dn.12xlarge"

pattern InstanceType_R5dn_16xlarge :: InstanceType
pattern InstanceType_R5dn_16xlarge = InstanceType' "r5dn.16xlarge"

pattern InstanceType_R5dn_24xlarge :: InstanceType
pattern InstanceType_R5dn_24xlarge = InstanceType' "r5dn.24xlarge"

pattern InstanceType_R5dn_2xlarge :: InstanceType
pattern InstanceType_R5dn_2xlarge = InstanceType' "r5dn.2xlarge"

pattern InstanceType_R5dn_4xlarge :: InstanceType
pattern InstanceType_R5dn_4xlarge = InstanceType' "r5dn.4xlarge"

pattern InstanceType_R5dn_8xlarge :: InstanceType
pattern InstanceType_R5dn_8xlarge = InstanceType' "r5dn.8xlarge"

pattern InstanceType_R5dn_large :: InstanceType
pattern InstanceType_R5dn_large = InstanceType' "r5dn.large"

pattern InstanceType_R5dn_metal :: InstanceType
pattern InstanceType_R5dn_metal = InstanceType' "r5dn.metal"

pattern InstanceType_R5dn_xlarge :: InstanceType
pattern InstanceType_R5dn_xlarge = InstanceType' "r5dn.xlarge"

pattern InstanceType_R5n_12xlarge :: InstanceType
pattern InstanceType_R5n_12xlarge = InstanceType' "r5n.12xlarge"

pattern InstanceType_R5n_16xlarge :: InstanceType
pattern InstanceType_R5n_16xlarge = InstanceType' "r5n.16xlarge"

pattern InstanceType_R5n_24xlarge :: InstanceType
pattern InstanceType_R5n_24xlarge = InstanceType' "r5n.24xlarge"

pattern InstanceType_R5n_2xlarge :: InstanceType
pattern InstanceType_R5n_2xlarge = InstanceType' "r5n.2xlarge"

pattern InstanceType_R5n_4xlarge :: InstanceType
pattern InstanceType_R5n_4xlarge = InstanceType' "r5n.4xlarge"

pattern InstanceType_R5n_8xlarge :: InstanceType
pattern InstanceType_R5n_8xlarge = InstanceType' "r5n.8xlarge"

pattern InstanceType_R5n_large :: InstanceType
pattern InstanceType_R5n_large = InstanceType' "r5n.large"

pattern InstanceType_R5n_metal :: InstanceType
pattern InstanceType_R5n_metal = InstanceType' "r5n.metal"

pattern InstanceType_R5n_xlarge :: InstanceType
pattern InstanceType_R5n_xlarge = InstanceType' "r5n.xlarge"

pattern InstanceType_R6a_12xlarge :: InstanceType
pattern InstanceType_R6a_12xlarge = InstanceType' "r6a.12xlarge"

pattern InstanceType_R6a_16xlarge :: InstanceType
pattern InstanceType_R6a_16xlarge = InstanceType' "r6a.16xlarge"

pattern InstanceType_R6a_24xlarge :: InstanceType
pattern InstanceType_R6a_24xlarge = InstanceType' "r6a.24xlarge"

pattern InstanceType_R6a_2xlarge :: InstanceType
pattern InstanceType_R6a_2xlarge = InstanceType' "r6a.2xlarge"

pattern InstanceType_R6a_32xlarge :: InstanceType
pattern InstanceType_R6a_32xlarge = InstanceType' "r6a.32xlarge"

pattern InstanceType_R6a_48xlarge :: InstanceType
pattern InstanceType_R6a_48xlarge = InstanceType' "r6a.48xlarge"

pattern InstanceType_R6a_4xlarge :: InstanceType
pattern InstanceType_R6a_4xlarge = InstanceType' "r6a.4xlarge"

pattern InstanceType_R6a_8xlarge :: InstanceType
pattern InstanceType_R6a_8xlarge = InstanceType' "r6a.8xlarge"

pattern InstanceType_R6a_large :: InstanceType
pattern InstanceType_R6a_large = InstanceType' "r6a.large"

pattern InstanceType_R6a_metal :: InstanceType
pattern InstanceType_R6a_metal = InstanceType' "r6a.metal"

pattern InstanceType_R6a_xlarge :: InstanceType
pattern InstanceType_R6a_xlarge = InstanceType' "r6a.xlarge"

pattern InstanceType_R6g_12xlarge :: InstanceType
pattern InstanceType_R6g_12xlarge = InstanceType' "r6g.12xlarge"

pattern InstanceType_R6g_16xlarge :: InstanceType
pattern InstanceType_R6g_16xlarge = InstanceType' "r6g.16xlarge"

pattern InstanceType_R6g_2xlarge :: InstanceType
pattern InstanceType_R6g_2xlarge = InstanceType' "r6g.2xlarge"

pattern InstanceType_R6g_4xlarge :: InstanceType
pattern InstanceType_R6g_4xlarge = InstanceType' "r6g.4xlarge"

pattern InstanceType_R6g_8xlarge :: InstanceType
pattern InstanceType_R6g_8xlarge = InstanceType' "r6g.8xlarge"

pattern InstanceType_R6g_large :: InstanceType
pattern InstanceType_R6g_large = InstanceType' "r6g.large"

pattern InstanceType_R6g_medium :: InstanceType
pattern InstanceType_R6g_medium = InstanceType' "r6g.medium"

pattern InstanceType_R6g_metal :: InstanceType
pattern InstanceType_R6g_metal = InstanceType' "r6g.metal"

pattern InstanceType_R6g_xlarge :: InstanceType
pattern InstanceType_R6g_xlarge = InstanceType' "r6g.xlarge"

pattern InstanceType_R6gd_12xlarge :: InstanceType
pattern InstanceType_R6gd_12xlarge = InstanceType' "r6gd.12xlarge"

pattern InstanceType_R6gd_16xlarge :: InstanceType
pattern InstanceType_R6gd_16xlarge = InstanceType' "r6gd.16xlarge"

pattern InstanceType_R6gd_2xlarge :: InstanceType
pattern InstanceType_R6gd_2xlarge = InstanceType' "r6gd.2xlarge"

pattern InstanceType_R6gd_4xlarge :: InstanceType
pattern InstanceType_R6gd_4xlarge = InstanceType' "r6gd.4xlarge"

pattern InstanceType_R6gd_8xlarge :: InstanceType
pattern InstanceType_R6gd_8xlarge = InstanceType' "r6gd.8xlarge"

pattern InstanceType_R6gd_large :: InstanceType
pattern InstanceType_R6gd_large = InstanceType' "r6gd.large"

pattern InstanceType_R6gd_medium :: InstanceType
pattern InstanceType_R6gd_medium = InstanceType' "r6gd.medium"

pattern InstanceType_R6gd_metal :: InstanceType
pattern InstanceType_R6gd_metal = InstanceType' "r6gd.metal"

pattern InstanceType_R6gd_xlarge :: InstanceType
pattern InstanceType_R6gd_xlarge = InstanceType' "r6gd.xlarge"

pattern InstanceType_R6i_12xlarge :: InstanceType
pattern InstanceType_R6i_12xlarge = InstanceType' "r6i.12xlarge"

pattern InstanceType_R6i_16xlarge :: InstanceType
pattern InstanceType_R6i_16xlarge = InstanceType' "r6i.16xlarge"

pattern InstanceType_R6i_24xlarge :: InstanceType
pattern InstanceType_R6i_24xlarge = InstanceType' "r6i.24xlarge"

pattern InstanceType_R6i_2xlarge :: InstanceType
pattern InstanceType_R6i_2xlarge = InstanceType' "r6i.2xlarge"

pattern InstanceType_R6i_32xlarge :: InstanceType
pattern InstanceType_R6i_32xlarge = InstanceType' "r6i.32xlarge"

pattern InstanceType_R6i_4xlarge :: InstanceType
pattern InstanceType_R6i_4xlarge = InstanceType' "r6i.4xlarge"

pattern InstanceType_R6i_8xlarge :: InstanceType
pattern InstanceType_R6i_8xlarge = InstanceType' "r6i.8xlarge"

pattern InstanceType_R6i_large :: InstanceType
pattern InstanceType_R6i_large = InstanceType' "r6i.large"

pattern InstanceType_R6i_metal :: InstanceType
pattern InstanceType_R6i_metal = InstanceType' "r6i.metal"

pattern InstanceType_R6i_xlarge :: InstanceType
pattern InstanceType_R6i_xlarge = InstanceType' "r6i.xlarge"

pattern InstanceType_R6id_12xlarge :: InstanceType
pattern InstanceType_R6id_12xlarge = InstanceType' "r6id.12xlarge"

pattern InstanceType_R6id_16xlarge :: InstanceType
pattern InstanceType_R6id_16xlarge = InstanceType' "r6id.16xlarge"

pattern InstanceType_R6id_24xlarge :: InstanceType
pattern InstanceType_R6id_24xlarge = InstanceType' "r6id.24xlarge"

pattern InstanceType_R6id_2xlarge :: InstanceType
pattern InstanceType_R6id_2xlarge = InstanceType' "r6id.2xlarge"

pattern InstanceType_R6id_32xlarge :: InstanceType
pattern InstanceType_R6id_32xlarge = InstanceType' "r6id.32xlarge"

pattern InstanceType_R6id_4xlarge :: InstanceType
pattern InstanceType_R6id_4xlarge = InstanceType' "r6id.4xlarge"

pattern InstanceType_R6id_8xlarge :: InstanceType
pattern InstanceType_R6id_8xlarge = InstanceType' "r6id.8xlarge"

pattern InstanceType_R6id_large :: InstanceType
pattern InstanceType_R6id_large = InstanceType' "r6id.large"

pattern InstanceType_R6id_metal :: InstanceType
pattern InstanceType_R6id_metal = InstanceType' "r6id.metal"

pattern InstanceType_R6id_xlarge :: InstanceType
pattern InstanceType_R6id_xlarge = InstanceType' "r6id.xlarge"

pattern InstanceType_T1_micro :: InstanceType
pattern InstanceType_T1_micro = InstanceType' "t1.micro"

pattern InstanceType_T2_2xlarge :: InstanceType
pattern InstanceType_T2_2xlarge = InstanceType' "t2.2xlarge"

pattern InstanceType_T2_large :: InstanceType
pattern InstanceType_T2_large = InstanceType' "t2.large"

pattern InstanceType_T2_medium :: InstanceType
pattern InstanceType_T2_medium = InstanceType' "t2.medium"

pattern InstanceType_T2_micro :: InstanceType
pattern InstanceType_T2_micro = InstanceType' "t2.micro"

pattern InstanceType_T2_nano :: InstanceType
pattern InstanceType_T2_nano = InstanceType' "t2.nano"

pattern InstanceType_T2_small :: InstanceType
pattern InstanceType_T2_small = InstanceType' "t2.small"

pattern InstanceType_T2_xlarge :: InstanceType
pattern InstanceType_T2_xlarge = InstanceType' "t2.xlarge"

pattern InstanceType_T3_2xlarge :: InstanceType
pattern InstanceType_T3_2xlarge = InstanceType' "t3.2xlarge"

pattern InstanceType_T3_large :: InstanceType
pattern InstanceType_T3_large = InstanceType' "t3.large"

pattern InstanceType_T3_medium :: InstanceType
pattern InstanceType_T3_medium = InstanceType' "t3.medium"

pattern InstanceType_T3_micro :: InstanceType
pattern InstanceType_T3_micro = InstanceType' "t3.micro"

pattern InstanceType_T3_nano :: InstanceType
pattern InstanceType_T3_nano = InstanceType' "t3.nano"

pattern InstanceType_T3_small :: InstanceType
pattern InstanceType_T3_small = InstanceType' "t3.small"

pattern InstanceType_T3_xlarge :: InstanceType
pattern InstanceType_T3_xlarge = InstanceType' "t3.xlarge"

pattern InstanceType_T3a_2xlarge :: InstanceType
pattern InstanceType_T3a_2xlarge = InstanceType' "t3a.2xlarge"

pattern InstanceType_T3a_large :: InstanceType
pattern InstanceType_T3a_large = InstanceType' "t3a.large"

pattern InstanceType_T3a_medium :: InstanceType
pattern InstanceType_T3a_medium = InstanceType' "t3a.medium"

pattern InstanceType_T3a_micro :: InstanceType
pattern InstanceType_T3a_micro = InstanceType' "t3a.micro"

pattern InstanceType_T3a_nano :: InstanceType
pattern InstanceType_T3a_nano = InstanceType' "t3a.nano"

pattern InstanceType_T3a_small :: InstanceType
pattern InstanceType_T3a_small = InstanceType' "t3a.small"

pattern InstanceType_T3a_xlarge :: InstanceType
pattern InstanceType_T3a_xlarge = InstanceType' "t3a.xlarge"

pattern InstanceType_T4g_2xlarge :: InstanceType
pattern InstanceType_T4g_2xlarge = InstanceType' "t4g.2xlarge"

pattern InstanceType_T4g_large :: InstanceType
pattern InstanceType_T4g_large = InstanceType' "t4g.large"

pattern InstanceType_T4g_medium :: InstanceType
pattern InstanceType_T4g_medium = InstanceType' "t4g.medium"

pattern InstanceType_T4g_micro :: InstanceType
pattern InstanceType_T4g_micro = InstanceType' "t4g.micro"

pattern InstanceType_T4g_nano :: InstanceType
pattern InstanceType_T4g_nano = InstanceType' "t4g.nano"

pattern InstanceType_T4g_small :: InstanceType
pattern InstanceType_T4g_small = InstanceType' "t4g.small"

pattern InstanceType_T4g_xlarge :: InstanceType
pattern InstanceType_T4g_xlarge = InstanceType' "t4g.xlarge"

pattern InstanceType_Trn1_2xlarge :: InstanceType
pattern InstanceType_Trn1_2xlarge = InstanceType' "trn1.2xlarge"

pattern InstanceType_Trn1_32xlarge :: InstanceType
pattern InstanceType_Trn1_32xlarge = InstanceType' "trn1.32xlarge"

pattern InstanceType_U_12tb1_112xlarge :: InstanceType
pattern InstanceType_U_12tb1_112xlarge = InstanceType' "u-12tb1.112xlarge"

pattern InstanceType_U_12tb1_metal :: InstanceType
pattern InstanceType_U_12tb1_metal = InstanceType' "u-12tb1.metal"

pattern InstanceType_U_18tb1_112xlarge :: InstanceType
pattern InstanceType_U_18tb1_112xlarge = InstanceType' "u-18tb1.112xlarge"

pattern InstanceType_U_18tb1_metal :: InstanceType
pattern InstanceType_U_18tb1_metal = InstanceType' "u-18tb1.metal"

pattern InstanceType_U_24tb1_112xlarge :: InstanceType
pattern InstanceType_U_24tb1_112xlarge = InstanceType' "u-24tb1.112xlarge"

pattern InstanceType_U_24tb1_metal :: InstanceType
pattern InstanceType_U_24tb1_metal = InstanceType' "u-24tb1.metal"

pattern InstanceType_U_3tb1_56xlarge :: InstanceType
pattern InstanceType_U_3tb1_56xlarge = InstanceType' "u-3tb1.56xlarge"

pattern InstanceType_U_6tb1_112xlarge :: InstanceType
pattern InstanceType_U_6tb1_112xlarge = InstanceType' "u-6tb1.112xlarge"

pattern InstanceType_U_6tb1_56xlarge :: InstanceType
pattern InstanceType_U_6tb1_56xlarge = InstanceType' "u-6tb1.56xlarge"

pattern InstanceType_U_6tb1_metal :: InstanceType
pattern InstanceType_U_6tb1_metal = InstanceType' "u-6tb1.metal"

pattern InstanceType_U_9tb1_112xlarge :: InstanceType
pattern InstanceType_U_9tb1_112xlarge = InstanceType' "u-9tb1.112xlarge"

pattern InstanceType_U_9tb1_metal :: InstanceType
pattern InstanceType_U_9tb1_metal = InstanceType' "u-9tb1.metal"

pattern InstanceType_Vt1_24xlarge :: InstanceType
pattern InstanceType_Vt1_24xlarge = InstanceType' "vt1.24xlarge"

pattern InstanceType_Vt1_3xlarge :: InstanceType
pattern InstanceType_Vt1_3xlarge = InstanceType' "vt1.3xlarge"

pattern InstanceType_Vt1_6xlarge :: InstanceType
pattern InstanceType_Vt1_6xlarge = InstanceType' "vt1.6xlarge"

pattern InstanceType_X1_16xlarge :: InstanceType
pattern InstanceType_X1_16xlarge = InstanceType' "x1.16xlarge"

pattern InstanceType_X1_32xlarge :: InstanceType
pattern InstanceType_X1_32xlarge = InstanceType' "x1.32xlarge"

pattern InstanceType_X1e_16xlarge :: InstanceType
pattern InstanceType_X1e_16xlarge = InstanceType' "x1e.16xlarge"

pattern InstanceType_X1e_2xlarge :: InstanceType
pattern InstanceType_X1e_2xlarge = InstanceType' "x1e.2xlarge"

pattern InstanceType_X1e_32xlarge :: InstanceType
pattern InstanceType_X1e_32xlarge = InstanceType' "x1e.32xlarge"

pattern InstanceType_X1e_4xlarge :: InstanceType
pattern InstanceType_X1e_4xlarge = InstanceType' "x1e.4xlarge"

pattern InstanceType_X1e_8xlarge :: InstanceType
pattern InstanceType_X1e_8xlarge = InstanceType' "x1e.8xlarge"

pattern InstanceType_X1e_xlarge :: InstanceType
pattern InstanceType_X1e_xlarge = InstanceType' "x1e.xlarge"

pattern InstanceType_X2gd_12xlarge :: InstanceType
pattern InstanceType_X2gd_12xlarge = InstanceType' "x2gd.12xlarge"

pattern InstanceType_X2gd_16xlarge :: InstanceType
pattern InstanceType_X2gd_16xlarge = InstanceType' "x2gd.16xlarge"

pattern InstanceType_X2gd_2xlarge :: InstanceType
pattern InstanceType_X2gd_2xlarge = InstanceType' "x2gd.2xlarge"

pattern InstanceType_X2gd_4xlarge :: InstanceType
pattern InstanceType_X2gd_4xlarge = InstanceType' "x2gd.4xlarge"

pattern InstanceType_X2gd_8xlarge :: InstanceType
pattern InstanceType_X2gd_8xlarge = InstanceType' "x2gd.8xlarge"

pattern InstanceType_X2gd_large :: InstanceType
pattern InstanceType_X2gd_large = InstanceType' "x2gd.large"

pattern InstanceType_X2gd_medium :: InstanceType
pattern InstanceType_X2gd_medium = InstanceType' "x2gd.medium"

pattern InstanceType_X2gd_metal :: InstanceType
pattern InstanceType_X2gd_metal = InstanceType' "x2gd.metal"

pattern InstanceType_X2gd_xlarge :: InstanceType
pattern InstanceType_X2gd_xlarge = InstanceType' "x2gd.xlarge"

pattern InstanceType_X2idn_16xlarge :: InstanceType
pattern InstanceType_X2idn_16xlarge = InstanceType' "x2idn.16xlarge"

pattern InstanceType_X2idn_24xlarge :: InstanceType
pattern InstanceType_X2idn_24xlarge = InstanceType' "x2idn.24xlarge"

pattern InstanceType_X2idn_32xlarge :: InstanceType
pattern InstanceType_X2idn_32xlarge = InstanceType' "x2idn.32xlarge"

pattern InstanceType_X2idn_metal :: InstanceType
pattern InstanceType_X2idn_metal = InstanceType' "x2idn.metal"

pattern InstanceType_X2iedn_16xlarge :: InstanceType
pattern InstanceType_X2iedn_16xlarge = InstanceType' "x2iedn.16xlarge"

pattern InstanceType_X2iedn_24xlarge :: InstanceType
pattern InstanceType_X2iedn_24xlarge = InstanceType' "x2iedn.24xlarge"

pattern InstanceType_X2iedn_2xlarge :: InstanceType
pattern InstanceType_X2iedn_2xlarge = InstanceType' "x2iedn.2xlarge"

pattern InstanceType_X2iedn_32xlarge :: InstanceType
pattern InstanceType_X2iedn_32xlarge = InstanceType' "x2iedn.32xlarge"

pattern InstanceType_X2iedn_4xlarge :: InstanceType
pattern InstanceType_X2iedn_4xlarge = InstanceType' "x2iedn.4xlarge"

pattern InstanceType_X2iedn_8xlarge :: InstanceType
pattern InstanceType_X2iedn_8xlarge = InstanceType' "x2iedn.8xlarge"

pattern InstanceType_X2iedn_metal :: InstanceType
pattern InstanceType_X2iedn_metal = InstanceType' "x2iedn.metal"

pattern InstanceType_X2iedn_xlarge :: InstanceType
pattern InstanceType_X2iedn_xlarge = InstanceType' "x2iedn.xlarge"

pattern InstanceType_X2iezn_12xlarge :: InstanceType
pattern InstanceType_X2iezn_12xlarge = InstanceType' "x2iezn.12xlarge"

pattern InstanceType_X2iezn_2xlarge :: InstanceType
pattern InstanceType_X2iezn_2xlarge = InstanceType' "x2iezn.2xlarge"

pattern InstanceType_X2iezn_4xlarge :: InstanceType
pattern InstanceType_X2iezn_4xlarge = InstanceType' "x2iezn.4xlarge"

pattern InstanceType_X2iezn_6xlarge :: InstanceType
pattern InstanceType_X2iezn_6xlarge = InstanceType' "x2iezn.6xlarge"

pattern InstanceType_X2iezn_8xlarge :: InstanceType
pattern InstanceType_X2iezn_8xlarge = InstanceType' "x2iezn.8xlarge"

pattern InstanceType_X2iezn_metal :: InstanceType
pattern InstanceType_X2iezn_metal = InstanceType' "x2iezn.metal"

pattern InstanceType_Z1d_12xlarge :: InstanceType
pattern InstanceType_Z1d_12xlarge = InstanceType' "z1d.12xlarge"

pattern InstanceType_Z1d_2xlarge :: InstanceType
pattern InstanceType_Z1d_2xlarge = InstanceType' "z1d.2xlarge"

pattern InstanceType_Z1d_3xlarge :: InstanceType
pattern InstanceType_Z1d_3xlarge = InstanceType' "z1d.3xlarge"

pattern InstanceType_Z1d_6xlarge :: InstanceType
pattern InstanceType_Z1d_6xlarge = InstanceType' "z1d.6xlarge"

pattern InstanceType_Z1d_large :: InstanceType
pattern InstanceType_Z1d_large = InstanceType' "z1d.large"

pattern InstanceType_Z1d_metal :: InstanceType
pattern InstanceType_Z1d_metal = InstanceType' "z1d.metal"

pattern InstanceType_Z1d_xlarge :: InstanceType
pattern InstanceType_Z1d_xlarge = InstanceType' "z1d.xlarge"

{-# COMPLETE
  InstanceType_A1_2xlarge,
  InstanceType_A1_4xlarge,
  InstanceType_A1_large,
  InstanceType_A1_medium,
  InstanceType_A1_metal,
  InstanceType_A1_xlarge,
  InstanceType_C1_medium,
  InstanceType_C1_xlarge,
  InstanceType_C3_2xlarge,
  InstanceType_C3_4xlarge,
  InstanceType_C3_8xlarge,
  InstanceType_C3_large,
  InstanceType_C3_xlarge,
  InstanceType_C4_2xlarge,
  InstanceType_C4_4xlarge,
  InstanceType_C4_8xlarge,
  InstanceType_C4_large,
  InstanceType_C4_xlarge,
  InstanceType_C5_12xlarge,
  InstanceType_C5_18xlarge,
  InstanceType_C5_24xlarge,
  InstanceType_C5_2xlarge,
  InstanceType_C5_4xlarge,
  InstanceType_C5_9xlarge,
  InstanceType_C5_large,
  InstanceType_C5_metal,
  InstanceType_C5_xlarge,
  InstanceType_C5a_12xlarge,
  InstanceType_C5a_16xlarge,
  InstanceType_C5a_24xlarge,
  InstanceType_C5a_2xlarge,
  InstanceType_C5a_4xlarge,
  InstanceType_C5a_8xlarge,
  InstanceType_C5a_large,
  InstanceType_C5a_xlarge,
  InstanceType_C5ad_12xlarge,
  InstanceType_C5ad_16xlarge,
  InstanceType_C5ad_24xlarge,
  InstanceType_C5ad_2xlarge,
  InstanceType_C5ad_4xlarge,
  InstanceType_C5ad_8xlarge,
  InstanceType_C5ad_large,
  InstanceType_C5ad_xlarge,
  InstanceType_C5d_12xlarge,
  InstanceType_C5d_18xlarge,
  InstanceType_C5d_24xlarge,
  InstanceType_C5d_2xlarge,
  InstanceType_C5d_4xlarge,
  InstanceType_C5d_9xlarge,
  InstanceType_C5d_large,
  InstanceType_C5d_metal,
  InstanceType_C5d_xlarge,
  InstanceType_C5n_18xlarge,
  InstanceType_C5n_2xlarge,
  InstanceType_C5n_4xlarge,
  InstanceType_C5n_9xlarge,
  InstanceType_C5n_large,
  InstanceType_C5n_metal,
  InstanceType_C5n_xlarge,
  InstanceType_C6a_12xlarge,
  InstanceType_C6a_16xlarge,
  InstanceType_C6a_24xlarge,
  InstanceType_C6a_2xlarge,
  InstanceType_C6a_32xlarge,
  InstanceType_C6a_48xlarge,
  InstanceType_C6a_4xlarge,
  InstanceType_C6a_8xlarge,
  InstanceType_C6a_large,
  InstanceType_C6a_metal,
  InstanceType_C6a_xlarge,
  InstanceType_C6g_12xlarge,
  InstanceType_C6g_16xlarge,
  InstanceType_C6g_2xlarge,
  InstanceType_C6g_4xlarge,
  InstanceType_C6g_8xlarge,
  InstanceType_C6g_large,
  InstanceType_C6g_medium,
  InstanceType_C6g_metal,
  InstanceType_C6g_xlarge,
  InstanceType_C6gd_12xlarge,
  InstanceType_C6gd_16xlarge,
  InstanceType_C6gd_2xlarge,
  InstanceType_C6gd_4xlarge,
  InstanceType_C6gd_8xlarge,
  InstanceType_C6gd_large,
  InstanceType_C6gd_medium,
  InstanceType_C6gd_metal,
  InstanceType_C6gd_xlarge,
  InstanceType_C6gn_12xlarge,
  InstanceType_C6gn_16xlarge,
  InstanceType_C6gn_2xlarge,
  InstanceType_C6gn_4xlarge,
  InstanceType_C6gn_8xlarge,
  InstanceType_C6gn_large,
  InstanceType_C6gn_medium,
  InstanceType_C6gn_xlarge,
  InstanceType_C6i_12xlarge,
  InstanceType_C6i_16xlarge,
  InstanceType_C6i_24xlarge,
  InstanceType_C6i_2xlarge,
  InstanceType_C6i_32xlarge,
  InstanceType_C6i_4xlarge,
  InstanceType_C6i_8xlarge,
  InstanceType_C6i_large,
  InstanceType_C6i_metal,
  InstanceType_C6i_xlarge,
  InstanceType_C6id_12xlarge,
  InstanceType_C6id_16xlarge,
  InstanceType_C6id_24xlarge,
  InstanceType_C6id_2xlarge,
  InstanceType_C6id_32xlarge,
  InstanceType_C6id_4xlarge,
  InstanceType_C6id_8xlarge,
  InstanceType_C6id_large,
  InstanceType_C6id_metal,
  InstanceType_C6id_xlarge,
  InstanceType_C7g_12xlarge,
  InstanceType_C7g_16xlarge,
  InstanceType_C7g_2xlarge,
  InstanceType_C7g_4xlarge,
  InstanceType_C7g_8xlarge,
  InstanceType_C7g_large,
  InstanceType_C7g_medium,
  InstanceType_C7g_xlarge,
  InstanceType_Cc1_4xlarge,
  InstanceType_Cc2_8xlarge,
  InstanceType_Cg1_4xlarge,
  InstanceType_Cr1_8xlarge,
  InstanceType_D2_2xlarge,
  InstanceType_D2_4xlarge,
  InstanceType_D2_8xlarge,
  InstanceType_D2_xlarge,
  InstanceType_D3_2xlarge,
  InstanceType_D3_4xlarge,
  InstanceType_D3_8xlarge,
  InstanceType_D3_xlarge,
  InstanceType_D3en_12xlarge,
  InstanceType_D3en_2xlarge,
  InstanceType_D3en_4xlarge,
  InstanceType_D3en_6xlarge,
  InstanceType_D3en_8xlarge,
  InstanceType_D3en_xlarge,
  InstanceType_Dl1_24xlarge,
  InstanceType_F1_16xlarge,
  InstanceType_F1_2xlarge,
  InstanceType_F1_4xlarge,
  InstanceType_G2_2xlarge,
  InstanceType_G2_8xlarge,
  InstanceType_G3_16xlarge,
  InstanceType_G3_4xlarge,
  InstanceType_G3_8xlarge,
  InstanceType_G3s_xlarge,
  InstanceType_G4ad_16xlarge,
  InstanceType_G4ad_2xlarge,
  InstanceType_G4ad_4xlarge,
  InstanceType_G4ad_8xlarge,
  InstanceType_G4ad_xlarge,
  InstanceType_G4dn_12xlarge,
  InstanceType_G4dn_16xlarge,
  InstanceType_G4dn_2xlarge,
  InstanceType_G4dn_4xlarge,
  InstanceType_G4dn_8xlarge,
  InstanceType_G4dn_metal,
  InstanceType_G4dn_xlarge,
  InstanceType_G5_12xlarge,
  InstanceType_G5_16xlarge,
  InstanceType_G5_24xlarge,
  InstanceType_G5_2xlarge,
  InstanceType_G5_48xlarge,
  InstanceType_G5_4xlarge,
  InstanceType_G5_8xlarge,
  InstanceType_G5_xlarge,
  InstanceType_G5g_16xlarge,
  InstanceType_G5g_2xlarge,
  InstanceType_G5g_4xlarge,
  InstanceType_G5g_8xlarge,
  InstanceType_G5g_metal,
  InstanceType_G5g_xlarge,
  InstanceType_H1_16xlarge,
  InstanceType_H1_2xlarge,
  InstanceType_H1_4xlarge,
  InstanceType_H1_8xlarge,
  InstanceType_Hi1_4xlarge,
  InstanceType_Hpc6a_48xlarge,
  InstanceType_Hpc6id_32xlarge,
  InstanceType_Hs1_8xlarge,
  InstanceType_I2_2xlarge,
  InstanceType_I2_4xlarge,
  InstanceType_I2_8xlarge,
  InstanceType_I2_xlarge,
  InstanceType_I3_16xlarge,
  InstanceType_I3_2xlarge,
  InstanceType_I3_4xlarge,
  InstanceType_I3_8xlarge,
  InstanceType_I3_large,
  InstanceType_I3_metal,
  InstanceType_I3_xlarge,
  InstanceType_I3en_12xlarge,
  InstanceType_I3en_24xlarge,
  InstanceType_I3en_2xlarge,
  InstanceType_I3en_3xlarge,
  InstanceType_I3en_6xlarge,
  InstanceType_I3en_large,
  InstanceType_I3en_metal,
  InstanceType_I3en_xlarge,
  InstanceType_I4i_16xlarge,
  InstanceType_I4i_2xlarge,
  InstanceType_I4i_32xlarge,
  InstanceType_I4i_4xlarge,
  InstanceType_I4i_8xlarge,
  InstanceType_I4i_large,
  InstanceType_I4i_metal,
  InstanceType_I4i_xlarge,
  InstanceType_Im4gn_16xlarge,
  InstanceType_Im4gn_2xlarge,
  InstanceType_Im4gn_4xlarge,
  InstanceType_Im4gn_8xlarge,
  InstanceType_Im4gn_large,
  InstanceType_Im4gn_xlarge,
  InstanceType_Inf1_24xlarge,
  InstanceType_Inf1_2xlarge,
  InstanceType_Inf1_6xlarge,
  InstanceType_Inf1_xlarge,
  InstanceType_Is4gen_2xlarge,
  InstanceType_Is4gen_4xlarge,
  InstanceType_Is4gen_8xlarge,
  InstanceType_Is4gen_large,
  InstanceType_Is4gen_medium,
  InstanceType_Is4gen_xlarge,
  InstanceType_M1_large,
  InstanceType_M1_medium,
  InstanceType_M1_small,
  InstanceType_M1_xlarge,
  InstanceType_M2_2xlarge,
  InstanceType_M2_4xlarge,
  InstanceType_M2_xlarge,
  InstanceType_M3_2xlarge,
  InstanceType_M3_large,
  InstanceType_M3_medium,
  InstanceType_M3_xlarge,
  InstanceType_M4_10xlarge,
  InstanceType_M4_16xlarge,
  InstanceType_M4_2xlarge,
  InstanceType_M4_4xlarge,
  InstanceType_M4_large,
  InstanceType_M4_xlarge,
  InstanceType_M5_12xlarge,
  InstanceType_M5_16xlarge,
  InstanceType_M5_24xlarge,
  InstanceType_M5_2xlarge,
  InstanceType_M5_4xlarge,
  InstanceType_M5_8xlarge,
  InstanceType_M5_large,
  InstanceType_M5_metal,
  InstanceType_M5_xlarge,
  InstanceType_M5a_12xlarge,
  InstanceType_M5a_16xlarge,
  InstanceType_M5a_24xlarge,
  InstanceType_M5a_2xlarge,
  InstanceType_M5a_4xlarge,
  InstanceType_M5a_8xlarge,
  InstanceType_M5a_large,
  InstanceType_M5a_xlarge,
  InstanceType_M5ad_12xlarge,
  InstanceType_M5ad_16xlarge,
  InstanceType_M5ad_24xlarge,
  InstanceType_M5ad_2xlarge,
  InstanceType_M5ad_4xlarge,
  InstanceType_M5ad_8xlarge,
  InstanceType_M5ad_large,
  InstanceType_M5ad_xlarge,
  InstanceType_M5d_12xlarge,
  InstanceType_M5d_16xlarge,
  InstanceType_M5d_24xlarge,
  InstanceType_M5d_2xlarge,
  InstanceType_M5d_4xlarge,
  InstanceType_M5d_8xlarge,
  InstanceType_M5d_large,
  InstanceType_M5d_metal,
  InstanceType_M5d_xlarge,
  InstanceType_M5dn_12xlarge,
  InstanceType_M5dn_16xlarge,
  InstanceType_M5dn_24xlarge,
  InstanceType_M5dn_2xlarge,
  InstanceType_M5dn_4xlarge,
  InstanceType_M5dn_8xlarge,
  InstanceType_M5dn_large,
  InstanceType_M5dn_metal,
  InstanceType_M5dn_xlarge,
  InstanceType_M5n_12xlarge,
  InstanceType_M5n_16xlarge,
  InstanceType_M5n_24xlarge,
  InstanceType_M5n_2xlarge,
  InstanceType_M5n_4xlarge,
  InstanceType_M5n_8xlarge,
  InstanceType_M5n_large,
  InstanceType_M5n_metal,
  InstanceType_M5n_xlarge,
  InstanceType_M5zn_12xlarge,
  InstanceType_M5zn_2xlarge,
  InstanceType_M5zn_3xlarge,
  InstanceType_M5zn_6xlarge,
  InstanceType_M5zn_large,
  InstanceType_M5zn_metal,
  InstanceType_M5zn_xlarge,
  InstanceType_M6a_12xlarge,
  InstanceType_M6a_16xlarge,
  InstanceType_M6a_24xlarge,
  InstanceType_M6a_2xlarge,
  InstanceType_M6a_32xlarge,
  InstanceType_M6a_48xlarge,
  InstanceType_M6a_4xlarge,
  InstanceType_M6a_8xlarge,
  InstanceType_M6a_large,
  InstanceType_M6a_metal,
  InstanceType_M6a_xlarge,
  InstanceType_M6g_12xlarge,
  InstanceType_M6g_16xlarge,
  InstanceType_M6g_2xlarge,
  InstanceType_M6g_4xlarge,
  InstanceType_M6g_8xlarge,
  InstanceType_M6g_large,
  InstanceType_M6g_medium,
  InstanceType_M6g_metal,
  InstanceType_M6g_xlarge,
  InstanceType_M6gd_12xlarge,
  InstanceType_M6gd_16xlarge,
  InstanceType_M6gd_2xlarge,
  InstanceType_M6gd_4xlarge,
  InstanceType_M6gd_8xlarge,
  InstanceType_M6gd_large,
  InstanceType_M6gd_medium,
  InstanceType_M6gd_metal,
  InstanceType_M6gd_xlarge,
  InstanceType_M6i_12xlarge,
  InstanceType_M6i_16xlarge,
  InstanceType_M6i_24xlarge,
  InstanceType_M6i_2xlarge,
  InstanceType_M6i_32xlarge,
  InstanceType_M6i_4xlarge,
  InstanceType_M6i_8xlarge,
  InstanceType_M6i_large,
  InstanceType_M6i_metal,
  InstanceType_M6i_xlarge,
  InstanceType_M6id_12xlarge,
  InstanceType_M6id_16xlarge,
  InstanceType_M6id_24xlarge,
  InstanceType_M6id_2xlarge,
  InstanceType_M6id_32xlarge,
  InstanceType_M6id_4xlarge,
  InstanceType_M6id_8xlarge,
  InstanceType_M6id_large,
  InstanceType_M6id_metal,
  InstanceType_M6id_xlarge,
  InstanceType_Mac1_metal,
  InstanceType_Mac2_metal,
  InstanceType_P2_16xlarge,
  InstanceType_P2_8xlarge,
  InstanceType_P2_xlarge,
  InstanceType_P3_16xlarge,
  InstanceType_P3_2xlarge,
  InstanceType_P3_8xlarge,
  InstanceType_P3dn_24xlarge,
  InstanceType_P4d_24xlarge,
  InstanceType_P4de_24xlarge,
  InstanceType_R3_2xlarge,
  InstanceType_R3_4xlarge,
  InstanceType_R3_8xlarge,
  InstanceType_R3_large,
  InstanceType_R3_xlarge,
  InstanceType_R4_16xlarge,
  InstanceType_R4_2xlarge,
  InstanceType_R4_4xlarge,
  InstanceType_R4_8xlarge,
  InstanceType_R4_large,
  InstanceType_R4_xlarge,
  InstanceType_R5_12xlarge,
  InstanceType_R5_16xlarge,
  InstanceType_R5_24xlarge,
  InstanceType_R5_2xlarge,
  InstanceType_R5_4xlarge,
  InstanceType_R5_8xlarge,
  InstanceType_R5_large,
  InstanceType_R5_metal,
  InstanceType_R5_xlarge,
  InstanceType_R5a_12xlarge,
  InstanceType_R5a_16xlarge,
  InstanceType_R5a_24xlarge,
  InstanceType_R5a_2xlarge,
  InstanceType_R5a_4xlarge,
  InstanceType_R5a_8xlarge,
  InstanceType_R5a_large,
  InstanceType_R5a_xlarge,
  InstanceType_R5ad_12xlarge,
  InstanceType_R5ad_16xlarge,
  InstanceType_R5ad_24xlarge,
  InstanceType_R5ad_2xlarge,
  InstanceType_R5ad_4xlarge,
  InstanceType_R5ad_8xlarge,
  InstanceType_R5ad_large,
  InstanceType_R5ad_xlarge,
  InstanceType_R5b_12xlarge,
  InstanceType_R5b_16xlarge,
  InstanceType_R5b_24xlarge,
  InstanceType_R5b_2xlarge,
  InstanceType_R5b_4xlarge,
  InstanceType_R5b_8xlarge,
  InstanceType_R5b_large,
  InstanceType_R5b_metal,
  InstanceType_R5b_xlarge,
  InstanceType_R5d_12xlarge,
  InstanceType_R5d_16xlarge,
  InstanceType_R5d_24xlarge,
  InstanceType_R5d_2xlarge,
  InstanceType_R5d_4xlarge,
  InstanceType_R5d_8xlarge,
  InstanceType_R5d_large,
  InstanceType_R5d_metal,
  InstanceType_R5d_xlarge,
  InstanceType_R5dn_12xlarge,
  InstanceType_R5dn_16xlarge,
  InstanceType_R5dn_24xlarge,
  InstanceType_R5dn_2xlarge,
  InstanceType_R5dn_4xlarge,
  InstanceType_R5dn_8xlarge,
  InstanceType_R5dn_large,
  InstanceType_R5dn_metal,
  InstanceType_R5dn_xlarge,
  InstanceType_R5n_12xlarge,
  InstanceType_R5n_16xlarge,
  InstanceType_R5n_24xlarge,
  InstanceType_R5n_2xlarge,
  InstanceType_R5n_4xlarge,
  InstanceType_R5n_8xlarge,
  InstanceType_R5n_large,
  InstanceType_R5n_metal,
  InstanceType_R5n_xlarge,
  InstanceType_R6a_12xlarge,
  InstanceType_R6a_16xlarge,
  InstanceType_R6a_24xlarge,
  InstanceType_R6a_2xlarge,
  InstanceType_R6a_32xlarge,
  InstanceType_R6a_48xlarge,
  InstanceType_R6a_4xlarge,
  InstanceType_R6a_8xlarge,
  InstanceType_R6a_large,
  InstanceType_R6a_metal,
  InstanceType_R6a_xlarge,
  InstanceType_R6g_12xlarge,
  InstanceType_R6g_16xlarge,
  InstanceType_R6g_2xlarge,
  InstanceType_R6g_4xlarge,
  InstanceType_R6g_8xlarge,
  InstanceType_R6g_large,
  InstanceType_R6g_medium,
  InstanceType_R6g_metal,
  InstanceType_R6g_xlarge,
  InstanceType_R6gd_12xlarge,
  InstanceType_R6gd_16xlarge,
  InstanceType_R6gd_2xlarge,
  InstanceType_R6gd_4xlarge,
  InstanceType_R6gd_8xlarge,
  InstanceType_R6gd_large,
  InstanceType_R6gd_medium,
  InstanceType_R6gd_metal,
  InstanceType_R6gd_xlarge,
  InstanceType_R6i_12xlarge,
  InstanceType_R6i_16xlarge,
  InstanceType_R6i_24xlarge,
  InstanceType_R6i_2xlarge,
  InstanceType_R6i_32xlarge,
  InstanceType_R6i_4xlarge,
  InstanceType_R6i_8xlarge,
  InstanceType_R6i_large,
  InstanceType_R6i_metal,
  InstanceType_R6i_xlarge,
  InstanceType_R6id_12xlarge,
  InstanceType_R6id_16xlarge,
  InstanceType_R6id_24xlarge,
  InstanceType_R6id_2xlarge,
  InstanceType_R6id_32xlarge,
  InstanceType_R6id_4xlarge,
  InstanceType_R6id_8xlarge,
  InstanceType_R6id_large,
  InstanceType_R6id_metal,
  InstanceType_R6id_xlarge,
  InstanceType_T1_micro,
  InstanceType_T2_2xlarge,
  InstanceType_T2_large,
  InstanceType_T2_medium,
  InstanceType_T2_micro,
  InstanceType_T2_nano,
  InstanceType_T2_small,
  InstanceType_T2_xlarge,
  InstanceType_T3_2xlarge,
  InstanceType_T3_large,
  InstanceType_T3_medium,
  InstanceType_T3_micro,
  InstanceType_T3_nano,
  InstanceType_T3_small,
  InstanceType_T3_xlarge,
  InstanceType_T3a_2xlarge,
  InstanceType_T3a_large,
  InstanceType_T3a_medium,
  InstanceType_T3a_micro,
  InstanceType_T3a_nano,
  InstanceType_T3a_small,
  InstanceType_T3a_xlarge,
  InstanceType_T4g_2xlarge,
  InstanceType_T4g_large,
  InstanceType_T4g_medium,
  InstanceType_T4g_micro,
  InstanceType_T4g_nano,
  InstanceType_T4g_small,
  InstanceType_T4g_xlarge,
  InstanceType_Trn1_2xlarge,
  InstanceType_Trn1_32xlarge,
  InstanceType_U_12tb1_112xlarge,
  InstanceType_U_12tb1_metal,
  InstanceType_U_18tb1_112xlarge,
  InstanceType_U_18tb1_metal,
  InstanceType_U_24tb1_112xlarge,
  InstanceType_U_24tb1_metal,
  InstanceType_U_3tb1_56xlarge,
  InstanceType_U_6tb1_112xlarge,
  InstanceType_U_6tb1_56xlarge,
  InstanceType_U_6tb1_metal,
  InstanceType_U_9tb1_112xlarge,
  InstanceType_U_9tb1_metal,
  InstanceType_Vt1_24xlarge,
  InstanceType_Vt1_3xlarge,
  InstanceType_Vt1_6xlarge,
  InstanceType_X1_16xlarge,
  InstanceType_X1_32xlarge,
  InstanceType_X1e_16xlarge,
  InstanceType_X1e_2xlarge,
  InstanceType_X1e_32xlarge,
  InstanceType_X1e_4xlarge,
  InstanceType_X1e_8xlarge,
  InstanceType_X1e_xlarge,
  InstanceType_X2gd_12xlarge,
  InstanceType_X2gd_16xlarge,
  InstanceType_X2gd_2xlarge,
  InstanceType_X2gd_4xlarge,
  InstanceType_X2gd_8xlarge,
  InstanceType_X2gd_large,
  InstanceType_X2gd_medium,
  InstanceType_X2gd_metal,
  InstanceType_X2gd_xlarge,
  InstanceType_X2idn_16xlarge,
  InstanceType_X2idn_24xlarge,
  InstanceType_X2idn_32xlarge,
  InstanceType_X2idn_metal,
  InstanceType_X2iedn_16xlarge,
  InstanceType_X2iedn_24xlarge,
  InstanceType_X2iedn_2xlarge,
  InstanceType_X2iedn_32xlarge,
  InstanceType_X2iedn_4xlarge,
  InstanceType_X2iedn_8xlarge,
  InstanceType_X2iedn_metal,
  InstanceType_X2iedn_xlarge,
  InstanceType_X2iezn_12xlarge,
  InstanceType_X2iezn_2xlarge,
  InstanceType_X2iezn_4xlarge,
  InstanceType_X2iezn_6xlarge,
  InstanceType_X2iezn_8xlarge,
  InstanceType_X2iezn_metal,
  InstanceType_Z1d_12xlarge,
  InstanceType_Z1d_2xlarge,
  InstanceType_Z1d_3xlarge,
  InstanceType_Z1d_6xlarge,
  InstanceType_Z1d_large,
  InstanceType_Z1d_metal,
  InstanceType_Z1d_xlarge,
  InstanceType'
  #-}
