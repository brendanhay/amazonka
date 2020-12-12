{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceType
  ( InstanceType
      ( InstanceType',
        A1_2XLarge,
        A1_4XLarge,
        A1_Large,
        A1_Medium,
        A1_Metal,
        A1_XLarge,
        C1_Medium,
        C1_XLarge,
        C3_2XLarge,
        C3_4XLarge,
        C3_8XLarge,
        C3_Large,
        C3_XLarge,
        C4_2XLarge,
        C4_4XLarge,
        C4_8XLarge,
        C4_Large,
        C4_XLarge,
        C5_12XLarge,
        C5_18XLarge,
        C5_24XLarge,
        C5_2XLarge,
        C5_4XLarge,
        C5_9XLarge,
        C5_Large,
        C5_Metal,
        C5_XLarge,
        C5a_12XLarge,
        C5a_16XLarge,
        C5a_24XLarge,
        C5a_2XLarge,
        C5a_4XLarge,
        C5a_8XLarge,
        C5a_Large,
        C5a_XLarge,
        C5ad_12XLarge,
        C5ad_16XLarge,
        C5ad_24XLarge,
        C5ad_2XLarge,
        C5ad_4XLarge,
        C5ad_8XLarge,
        C5ad_Large,
        C5ad_XLarge,
        C5d_12XLarge,
        C5d_18XLarge,
        C5d_24XLarge,
        C5d_2XLarge,
        C5d_4XLarge,
        C5d_9XLarge,
        C5d_Large,
        C5d_Metal,
        C5d_XLarge,
        C5n_18XLarge,
        C5n_2XLarge,
        C5n_4XLarge,
        C5n_9XLarge,
        C5n_Large,
        C5n_XLarge,
        C6g_12XLarge,
        C6g_16XLarge,
        C6g_2XLarge,
        C6g_4XLarge,
        C6g_8XLarge,
        C6g_Large,
        C6g_Medium,
        C6g_Metal,
        C6g_XLarge,
        C6gd_12XLarge,
        C6gd_16XLarge,
        C6gd_2XLarge,
        C6gd_4XLarge,
        C6gd_8XLarge,
        C6gd_Large,
        C6gd_Medium,
        C6gd_Metal,
        C6gd_XLarge,
        CC1_4XLarge,
        CC2_8XLarge,
        CG1_4XLarge,
        CR1_8XLarge,
        D2_2XLarge,
        D2_4XLarge,
        D2_8XLarge,
        D2_XLarge,
        F1_16XLarge,
        F1_2XLarge,
        F1_4XLarge,
        G2_2XLarge,
        G2_8XLarge,
        G3_16XLarge,
        G3_4XLarge,
        G3_8XLarge,
        G3s_XLarge,
        G4dn_12XLarge,
        G4dn_16XLarge,
        G4dn_2XLarge,
        G4dn_4XLarge,
        G4dn_8XLarge,
        G4dn_Metal,
        G4dn_XLarge,
        H1_16XLarge,
        H1_2XLarge,
        H1_4XLarge,
        H1_8XLarge,
        HI1_4XLarge,
        HS1_8XLarge,
        I2_2XLarge,
        I2_4XLarge,
        I2_8XLarge,
        I2_XLarge,
        I3_16XLarge,
        I3_2XLarge,
        I3_4XLarge,
        I3_8XLarge,
        I3_Large,
        I3_Metal,
        I3_XLarge,
        I3en_12XLarge,
        I3en_24XLarge,
        I3en_2XLarge,
        I3en_3XLarge,
        I3en_6XLarge,
        I3en_Large,
        I3en_Metal,
        I3en_XLarge,
        INF1_24XLarge,
        INF1_2XLarge,
        INF1_6XLarge,
        INF1_XLarge,
        M1_Large,
        M1_Medium,
        M1_Small,
        M1_XLarge,
        M2_2XLarge,
        M2_4XLarge,
        M2_XLarge,
        M3_2XLarge,
        M3_Large,
        M3_Medium,
        M3_XLarge,
        M4_10XLarge,
        M4_16XLarge,
        M4_2XLarge,
        M4_4XLarge,
        M4_Large,
        M4_XLarge,
        M5_12XLarge,
        M5_16XLarge,
        M5_24XLarge,
        M5_2XLarge,
        M5_4XLarge,
        M5_8XLarge,
        M5_Large,
        M5_Metal,
        M5_XLarge,
        M5a_12XLarge,
        M5a_16XLarge,
        M5a_24XLarge,
        M5a_2XLarge,
        M5a_4XLarge,
        M5a_8XLarge,
        M5a_Large,
        M5a_XLarge,
        M5ad_12XLarge,
        M5ad_16XLarge,
        M5ad_24XLarge,
        M5ad_2XLarge,
        M5ad_4XLarge,
        M5ad_8XLarge,
        M5ad_Large,
        M5ad_XLarge,
        M5d_12XLarge,
        M5d_16XLarge,
        M5d_24XLarge,
        M5d_2XLarge,
        M5d_4XLarge,
        M5d_8XLarge,
        M5d_Large,
        M5d_Metal,
        M5d_XLarge,
        M5dn_12XLarge,
        M5dn_16XLarge,
        M5dn_24XLarge,
        M5dn_2XLarge,
        M5dn_4XLarge,
        M5dn_8XLarge,
        M5dn_Large,
        M5dn_XLarge,
        M5n_12XLarge,
        M5n_16XLarge,
        M5n_24XLarge,
        M5n_2XLarge,
        M5n_4XLarge,
        M5n_8XLarge,
        M5n_Large,
        M5n_XLarge,
        M6g_12XLarge,
        M6g_16XLarge,
        M6g_2XLarge,
        M6g_4XLarge,
        M6g_8XLarge,
        M6g_Large,
        M6g_Medium,
        M6g_Metal,
        M6g_XLarge,
        M6gd_12XLarge,
        M6gd_16XLarge,
        M6gd_2XLarge,
        M6gd_4XLarge,
        M6gd_8XLarge,
        M6gd_Large,
        M6gd_Medium,
        M6gd_Metal,
        M6gd_XLarge,
        P2_16XLarge,
        P2_8XLarge,
        P2_XLarge,
        P3_16XLarge,
        P3_2XLarge,
        P3_8XLarge,
        P3dn_24XLarge,
        P4d_24XLarge,
        R3_2XLarge,
        R3_4XLarge,
        R3_8XLarge,
        R3_Large,
        R3_XLarge,
        R4_16XLarge,
        R4_2XLarge,
        R4_4XLarge,
        R4_8XLarge,
        R4_Large,
        R4_XLarge,
        R5_12XLarge,
        R5_16XLarge,
        R5_24XLarge,
        R5_2XLarge,
        R5_4XLarge,
        R5_8XLarge,
        R5_Large,
        R5_Metal,
        R5_XLarge,
        R5a_12XLarge,
        R5a_16XLarge,
        R5a_24XLarge,
        R5a_2XLarge,
        R5a_4XLarge,
        R5a_8XLarge,
        R5a_Large,
        R5a_XLarge,
        R5ad_12XLarge,
        R5ad_16XLarge,
        R5ad_24XLarge,
        R5ad_2XLarge,
        R5ad_4XLarge,
        R5ad_8XLarge,
        R5ad_Large,
        R5ad_XLarge,
        R5d_12XLarge,
        R5d_16XLarge,
        R5d_24XLarge,
        R5d_2XLarge,
        R5d_4XLarge,
        R5d_8XLarge,
        R5d_Large,
        R5d_Metal,
        R5d_XLarge,
        R5dn_12XLarge,
        R5dn_16XLarge,
        R5dn_24XLarge,
        R5dn_2XLarge,
        R5dn_4XLarge,
        R5dn_8XLarge,
        R5dn_Large,
        R5dn_XLarge,
        R5n_12XLarge,
        R5n_16XLarge,
        R5n_24XLarge,
        R5n_2XLarge,
        R5n_4XLarge,
        R5n_8XLarge,
        R5n_Large,
        R5n_XLarge,
        R6g_12XLarge,
        R6g_16XLarge,
        R6g_2XLarge,
        R6g_4XLarge,
        R6g_8XLarge,
        R6g_Large,
        R6g_Medium,
        R6g_Metal,
        R6g_XLarge,
        R6gd_12XLarge,
        R6gd_16XLarge,
        R6gd_2XLarge,
        R6gd_4XLarge,
        R6gd_8XLarge,
        R6gd_Large,
        R6gd_Medium,
        R6gd_Metal,
        R6gd_XLarge,
        T1_Micro,
        T2_2XLarge,
        T2_Large,
        T2_Medium,
        T2_Micro,
        T2_Nano,
        T2_Small,
        T2_XLarge,
        T3_2XLarge,
        T3_Large,
        T3_Medium,
        T3_Micro,
        T3_Nano,
        T3_Small,
        T3_XLarge,
        T3a_2XLarge,
        T3a_Large,
        T3a_Medium,
        T3a_Micro,
        T3a_Nano,
        T3a_Small,
        T3a_XLarge,
        T4g_2XLarge,
        T4g_Large,
        T4g_Medium,
        T4g_Micro,
        T4g_Nano,
        T4g_Small,
        T4g_XLarge,
        U12TB1_Metal,
        U18TB1_Metal,
        U24TB1_Metal,
        U6TB1_Metal,
        U9TB1_Metal,
        X1_16XLarge,
        X1_32XLarge,
        X1e_16XLarge,
        X1e_2XLarge,
        X1e_32XLarge,
        X1e_4XLarge,
        X1e_8XLarge,
        X1e_XLarge,
        Z1d_12XLarge,
        Z1d_2XLarge,
        Z1d_3XLarge,
        Z1d_6XLarge,
        Z1d_Large,
        Z1d_Metal,
        Z1d_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceType = InstanceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern A1_2XLarge :: InstanceType
pattern A1_2XLarge = InstanceType' "a1.2xlarge"

pattern A1_4XLarge :: InstanceType
pattern A1_4XLarge = InstanceType' "a1.4xlarge"

pattern A1_Large :: InstanceType
pattern A1_Large = InstanceType' "a1.large"

pattern A1_Medium :: InstanceType
pattern A1_Medium = InstanceType' "a1.medium"

pattern A1_Metal :: InstanceType
pattern A1_Metal = InstanceType' "a1.metal"

pattern A1_XLarge :: InstanceType
pattern A1_XLarge = InstanceType' "a1.xlarge"

pattern C1_Medium :: InstanceType
pattern C1_Medium = InstanceType' "c1.medium"

pattern C1_XLarge :: InstanceType
pattern C1_XLarge = InstanceType' "c1.xlarge"

pattern C3_2XLarge :: InstanceType
pattern C3_2XLarge = InstanceType' "c3.2xlarge"

pattern C3_4XLarge :: InstanceType
pattern C3_4XLarge = InstanceType' "c3.4xlarge"

pattern C3_8XLarge :: InstanceType
pattern C3_8XLarge = InstanceType' "c3.8xlarge"

pattern C3_Large :: InstanceType
pattern C3_Large = InstanceType' "c3.large"

pattern C3_XLarge :: InstanceType
pattern C3_XLarge = InstanceType' "c3.xlarge"

pattern C4_2XLarge :: InstanceType
pattern C4_2XLarge = InstanceType' "c4.2xlarge"

pattern C4_4XLarge :: InstanceType
pattern C4_4XLarge = InstanceType' "c4.4xlarge"

pattern C4_8XLarge :: InstanceType
pattern C4_8XLarge = InstanceType' "c4.8xlarge"

pattern C4_Large :: InstanceType
pattern C4_Large = InstanceType' "c4.large"

pattern C4_XLarge :: InstanceType
pattern C4_XLarge = InstanceType' "c4.xlarge"

pattern C5_12XLarge :: InstanceType
pattern C5_12XLarge = InstanceType' "c5.12xlarge"

pattern C5_18XLarge :: InstanceType
pattern C5_18XLarge = InstanceType' "c5.18xlarge"

pattern C5_24XLarge :: InstanceType
pattern C5_24XLarge = InstanceType' "c5.24xlarge"

pattern C5_2XLarge :: InstanceType
pattern C5_2XLarge = InstanceType' "c5.2xlarge"

pattern C5_4XLarge :: InstanceType
pattern C5_4XLarge = InstanceType' "c5.4xlarge"

pattern C5_9XLarge :: InstanceType
pattern C5_9XLarge = InstanceType' "c5.9xlarge"

pattern C5_Large :: InstanceType
pattern C5_Large = InstanceType' "c5.large"

pattern C5_Metal :: InstanceType
pattern C5_Metal = InstanceType' "c5.metal"

pattern C5_XLarge :: InstanceType
pattern C5_XLarge = InstanceType' "c5.xlarge"

pattern C5a_12XLarge :: InstanceType
pattern C5a_12XLarge = InstanceType' "c5a.12xlarge"

pattern C5a_16XLarge :: InstanceType
pattern C5a_16XLarge = InstanceType' "c5a.16xlarge"

pattern C5a_24XLarge :: InstanceType
pattern C5a_24XLarge = InstanceType' "c5a.24xlarge"

pattern C5a_2XLarge :: InstanceType
pattern C5a_2XLarge = InstanceType' "c5a.2xlarge"

pattern C5a_4XLarge :: InstanceType
pattern C5a_4XLarge = InstanceType' "c5a.4xlarge"

pattern C5a_8XLarge :: InstanceType
pattern C5a_8XLarge = InstanceType' "c5a.8xlarge"

pattern C5a_Large :: InstanceType
pattern C5a_Large = InstanceType' "c5a.large"

pattern C5a_XLarge :: InstanceType
pattern C5a_XLarge = InstanceType' "c5a.xlarge"

pattern C5ad_12XLarge :: InstanceType
pattern C5ad_12XLarge = InstanceType' "c5ad.12xlarge"

pattern C5ad_16XLarge :: InstanceType
pattern C5ad_16XLarge = InstanceType' "c5ad.16xlarge"

pattern C5ad_24XLarge :: InstanceType
pattern C5ad_24XLarge = InstanceType' "c5ad.24xlarge"

pattern C5ad_2XLarge :: InstanceType
pattern C5ad_2XLarge = InstanceType' "c5ad.2xlarge"

pattern C5ad_4XLarge :: InstanceType
pattern C5ad_4XLarge = InstanceType' "c5ad.4xlarge"

pattern C5ad_8XLarge :: InstanceType
pattern C5ad_8XLarge = InstanceType' "c5ad.8xlarge"

pattern C5ad_Large :: InstanceType
pattern C5ad_Large = InstanceType' "c5ad.large"

pattern C5ad_XLarge :: InstanceType
pattern C5ad_XLarge = InstanceType' "c5ad.xlarge"

pattern C5d_12XLarge :: InstanceType
pattern C5d_12XLarge = InstanceType' "c5d.12xlarge"

pattern C5d_18XLarge :: InstanceType
pattern C5d_18XLarge = InstanceType' "c5d.18xlarge"

pattern C5d_24XLarge :: InstanceType
pattern C5d_24XLarge = InstanceType' "c5d.24xlarge"

pattern C5d_2XLarge :: InstanceType
pattern C5d_2XLarge = InstanceType' "c5d.2xlarge"

pattern C5d_4XLarge :: InstanceType
pattern C5d_4XLarge = InstanceType' "c5d.4xlarge"

pattern C5d_9XLarge :: InstanceType
pattern C5d_9XLarge = InstanceType' "c5d.9xlarge"

pattern C5d_Large :: InstanceType
pattern C5d_Large = InstanceType' "c5d.large"

pattern C5d_Metal :: InstanceType
pattern C5d_Metal = InstanceType' "c5d.metal"

pattern C5d_XLarge :: InstanceType
pattern C5d_XLarge = InstanceType' "c5d.xlarge"

pattern C5n_18XLarge :: InstanceType
pattern C5n_18XLarge = InstanceType' "c5n.18xlarge"

pattern C5n_2XLarge :: InstanceType
pattern C5n_2XLarge = InstanceType' "c5n.2xlarge"

pattern C5n_4XLarge :: InstanceType
pattern C5n_4XLarge = InstanceType' "c5n.4xlarge"

pattern C5n_9XLarge :: InstanceType
pattern C5n_9XLarge = InstanceType' "c5n.9xlarge"

pattern C5n_Large :: InstanceType
pattern C5n_Large = InstanceType' "c5n.large"

pattern C5n_XLarge :: InstanceType
pattern C5n_XLarge = InstanceType' "c5n.xlarge"

pattern C6g_12XLarge :: InstanceType
pattern C6g_12XLarge = InstanceType' "c6g.12xlarge"

pattern C6g_16XLarge :: InstanceType
pattern C6g_16XLarge = InstanceType' "c6g.16xlarge"

pattern C6g_2XLarge :: InstanceType
pattern C6g_2XLarge = InstanceType' "c6g.2xlarge"

pattern C6g_4XLarge :: InstanceType
pattern C6g_4XLarge = InstanceType' "c6g.4xlarge"

pattern C6g_8XLarge :: InstanceType
pattern C6g_8XLarge = InstanceType' "c6g.8xlarge"

pattern C6g_Large :: InstanceType
pattern C6g_Large = InstanceType' "c6g.large"

pattern C6g_Medium :: InstanceType
pattern C6g_Medium = InstanceType' "c6g.medium"

pattern C6g_Metal :: InstanceType
pattern C6g_Metal = InstanceType' "c6g.metal"

pattern C6g_XLarge :: InstanceType
pattern C6g_XLarge = InstanceType' "c6g.xlarge"

pattern C6gd_12XLarge :: InstanceType
pattern C6gd_12XLarge = InstanceType' "c6gd.12xlarge"

pattern C6gd_16XLarge :: InstanceType
pattern C6gd_16XLarge = InstanceType' "c6gd.16xlarge"

pattern C6gd_2XLarge :: InstanceType
pattern C6gd_2XLarge = InstanceType' "c6gd.2xlarge"

pattern C6gd_4XLarge :: InstanceType
pattern C6gd_4XLarge = InstanceType' "c6gd.4xlarge"

pattern C6gd_8XLarge :: InstanceType
pattern C6gd_8XLarge = InstanceType' "c6gd.8xlarge"

pattern C6gd_Large :: InstanceType
pattern C6gd_Large = InstanceType' "c6gd.large"

pattern C6gd_Medium :: InstanceType
pattern C6gd_Medium = InstanceType' "c6gd.medium"

pattern C6gd_Metal :: InstanceType
pattern C6gd_Metal = InstanceType' "c6gd.metal"

pattern C6gd_XLarge :: InstanceType
pattern C6gd_XLarge = InstanceType' "c6gd.xlarge"

pattern CC1_4XLarge :: InstanceType
pattern CC1_4XLarge = InstanceType' "cc1.4xlarge"

pattern CC2_8XLarge :: InstanceType
pattern CC2_8XLarge = InstanceType' "cc2.8xlarge"

pattern CG1_4XLarge :: InstanceType
pattern CG1_4XLarge = InstanceType' "cg1.4xlarge"

pattern CR1_8XLarge :: InstanceType
pattern CR1_8XLarge = InstanceType' "cr1.8xlarge"

pattern D2_2XLarge :: InstanceType
pattern D2_2XLarge = InstanceType' "d2.2xlarge"

pattern D2_4XLarge :: InstanceType
pattern D2_4XLarge = InstanceType' "d2.4xlarge"

pattern D2_8XLarge :: InstanceType
pattern D2_8XLarge = InstanceType' "d2.8xlarge"

pattern D2_XLarge :: InstanceType
pattern D2_XLarge = InstanceType' "d2.xlarge"

pattern F1_16XLarge :: InstanceType
pattern F1_16XLarge = InstanceType' "f1.16xlarge"

pattern F1_2XLarge :: InstanceType
pattern F1_2XLarge = InstanceType' "f1.2xlarge"

pattern F1_4XLarge :: InstanceType
pattern F1_4XLarge = InstanceType' "f1.4xlarge"

pattern G2_2XLarge :: InstanceType
pattern G2_2XLarge = InstanceType' "g2.2xlarge"

pattern G2_8XLarge :: InstanceType
pattern G2_8XLarge = InstanceType' "g2.8xlarge"

pattern G3_16XLarge :: InstanceType
pattern G3_16XLarge = InstanceType' "g3.16xlarge"

pattern G3_4XLarge :: InstanceType
pattern G3_4XLarge = InstanceType' "g3.4xlarge"

pattern G3_8XLarge :: InstanceType
pattern G3_8XLarge = InstanceType' "g3.8xlarge"

pattern G3s_XLarge :: InstanceType
pattern G3s_XLarge = InstanceType' "g3s.xlarge"

pattern G4dn_12XLarge :: InstanceType
pattern G4dn_12XLarge = InstanceType' "g4dn.12xlarge"

pattern G4dn_16XLarge :: InstanceType
pattern G4dn_16XLarge = InstanceType' "g4dn.16xlarge"

pattern G4dn_2XLarge :: InstanceType
pattern G4dn_2XLarge = InstanceType' "g4dn.2xlarge"

pattern G4dn_4XLarge :: InstanceType
pattern G4dn_4XLarge = InstanceType' "g4dn.4xlarge"

pattern G4dn_8XLarge :: InstanceType
pattern G4dn_8XLarge = InstanceType' "g4dn.8xlarge"

pattern G4dn_Metal :: InstanceType
pattern G4dn_Metal = InstanceType' "g4dn.metal"

pattern G4dn_XLarge :: InstanceType
pattern G4dn_XLarge = InstanceType' "g4dn.xlarge"

pattern H1_16XLarge :: InstanceType
pattern H1_16XLarge = InstanceType' "h1.16xlarge"

pattern H1_2XLarge :: InstanceType
pattern H1_2XLarge = InstanceType' "h1.2xlarge"

pattern H1_4XLarge :: InstanceType
pattern H1_4XLarge = InstanceType' "h1.4xlarge"

pattern H1_8XLarge :: InstanceType
pattern H1_8XLarge = InstanceType' "h1.8xlarge"

pattern HI1_4XLarge :: InstanceType
pattern HI1_4XLarge = InstanceType' "hi1.4xlarge"

pattern HS1_8XLarge :: InstanceType
pattern HS1_8XLarge = InstanceType' "hs1.8xlarge"

pattern I2_2XLarge :: InstanceType
pattern I2_2XLarge = InstanceType' "i2.2xlarge"

pattern I2_4XLarge :: InstanceType
pattern I2_4XLarge = InstanceType' "i2.4xlarge"

pattern I2_8XLarge :: InstanceType
pattern I2_8XLarge = InstanceType' "i2.8xlarge"

pattern I2_XLarge :: InstanceType
pattern I2_XLarge = InstanceType' "i2.xlarge"

pattern I3_16XLarge :: InstanceType
pattern I3_16XLarge = InstanceType' "i3.16xlarge"

pattern I3_2XLarge :: InstanceType
pattern I3_2XLarge = InstanceType' "i3.2xlarge"

pattern I3_4XLarge :: InstanceType
pattern I3_4XLarge = InstanceType' "i3.4xlarge"

pattern I3_8XLarge :: InstanceType
pattern I3_8XLarge = InstanceType' "i3.8xlarge"

pattern I3_Large :: InstanceType
pattern I3_Large = InstanceType' "i3.large"

pattern I3_Metal :: InstanceType
pattern I3_Metal = InstanceType' "i3.metal"

pattern I3_XLarge :: InstanceType
pattern I3_XLarge = InstanceType' "i3.xlarge"

pattern I3en_12XLarge :: InstanceType
pattern I3en_12XLarge = InstanceType' "i3en.12xlarge"

pattern I3en_24XLarge :: InstanceType
pattern I3en_24XLarge = InstanceType' "i3en.24xlarge"

pattern I3en_2XLarge :: InstanceType
pattern I3en_2XLarge = InstanceType' "i3en.2xlarge"

pattern I3en_3XLarge :: InstanceType
pattern I3en_3XLarge = InstanceType' "i3en.3xlarge"

pattern I3en_6XLarge :: InstanceType
pattern I3en_6XLarge = InstanceType' "i3en.6xlarge"

pattern I3en_Large :: InstanceType
pattern I3en_Large = InstanceType' "i3en.large"

pattern I3en_Metal :: InstanceType
pattern I3en_Metal = InstanceType' "i3en.metal"

pattern I3en_XLarge :: InstanceType
pattern I3en_XLarge = InstanceType' "i3en.xlarge"

pattern INF1_24XLarge :: InstanceType
pattern INF1_24XLarge = InstanceType' "inf1.24xlarge"

pattern INF1_2XLarge :: InstanceType
pattern INF1_2XLarge = InstanceType' "inf1.2xlarge"

pattern INF1_6XLarge :: InstanceType
pattern INF1_6XLarge = InstanceType' "inf1.6xlarge"

pattern INF1_XLarge :: InstanceType
pattern INF1_XLarge = InstanceType' "inf1.xlarge"

pattern M1_Large :: InstanceType
pattern M1_Large = InstanceType' "m1.large"

pattern M1_Medium :: InstanceType
pattern M1_Medium = InstanceType' "m1.medium"

pattern M1_Small :: InstanceType
pattern M1_Small = InstanceType' "m1.small"

pattern M1_XLarge :: InstanceType
pattern M1_XLarge = InstanceType' "m1.xlarge"

pattern M2_2XLarge :: InstanceType
pattern M2_2XLarge = InstanceType' "m2.2xlarge"

pattern M2_4XLarge :: InstanceType
pattern M2_4XLarge = InstanceType' "m2.4xlarge"

pattern M2_XLarge :: InstanceType
pattern M2_XLarge = InstanceType' "m2.xlarge"

pattern M3_2XLarge :: InstanceType
pattern M3_2XLarge = InstanceType' "m3.2xlarge"

pattern M3_Large :: InstanceType
pattern M3_Large = InstanceType' "m3.large"

pattern M3_Medium :: InstanceType
pattern M3_Medium = InstanceType' "m3.medium"

pattern M3_XLarge :: InstanceType
pattern M3_XLarge = InstanceType' "m3.xlarge"

pattern M4_10XLarge :: InstanceType
pattern M4_10XLarge = InstanceType' "m4.10xlarge"

pattern M4_16XLarge :: InstanceType
pattern M4_16XLarge = InstanceType' "m4.16xlarge"

pattern M4_2XLarge :: InstanceType
pattern M4_2XLarge = InstanceType' "m4.2xlarge"

pattern M4_4XLarge :: InstanceType
pattern M4_4XLarge = InstanceType' "m4.4xlarge"

pattern M4_Large :: InstanceType
pattern M4_Large = InstanceType' "m4.large"

pattern M4_XLarge :: InstanceType
pattern M4_XLarge = InstanceType' "m4.xlarge"

pattern M5_12XLarge :: InstanceType
pattern M5_12XLarge = InstanceType' "m5.12xlarge"

pattern M5_16XLarge :: InstanceType
pattern M5_16XLarge = InstanceType' "m5.16xlarge"

pattern M5_24XLarge :: InstanceType
pattern M5_24XLarge = InstanceType' "m5.24xlarge"

pattern M5_2XLarge :: InstanceType
pattern M5_2XLarge = InstanceType' "m5.2xlarge"

pattern M5_4XLarge :: InstanceType
pattern M5_4XLarge = InstanceType' "m5.4xlarge"

pattern M5_8XLarge :: InstanceType
pattern M5_8XLarge = InstanceType' "m5.8xlarge"

pattern M5_Large :: InstanceType
pattern M5_Large = InstanceType' "m5.large"

pattern M5_Metal :: InstanceType
pattern M5_Metal = InstanceType' "m5.metal"

pattern M5_XLarge :: InstanceType
pattern M5_XLarge = InstanceType' "m5.xlarge"

pattern M5a_12XLarge :: InstanceType
pattern M5a_12XLarge = InstanceType' "m5a.12xlarge"

pattern M5a_16XLarge :: InstanceType
pattern M5a_16XLarge = InstanceType' "m5a.16xlarge"

pattern M5a_24XLarge :: InstanceType
pattern M5a_24XLarge = InstanceType' "m5a.24xlarge"

pattern M5a_2XLarge :: InstanceType
pattern M5a_2XLarge = InstanceType' "m5a.2xlarge"

pattern M5a_4XLarge :: InstanceType
pattern M5a_4XLarge = InstanceType' "m5a.4xlarge"

pattern M5a_8XLarge :: InstanceType
pattern M5a_8XLarge = InstanceType' "m5a.8xlarge"

pattern M5a_Large :: InstanceType
pattern M5a_Large = InstanceType' "m5a.large"

pattern M5a_XLarge :: InstanceType
pattern M5a_XLarge = InstanceType' "m5a.xlarge"

pattern M5ad_12XLarge :: InstanceType
pattern M5ad_12XLarge = InstanceType' "m5ad.12xlarge"

pattern M5ad_16XLarge :: InstanceType
pattern M5ad_16XLarge = InstanceType' "m5ad.16xlarge"

pattern M5ad_24XLarge :: InstanceType
pattern M5ad_24XLarge = InstanceType' "m5ad.24xlarge"

pattern M5ad_2XLarge :: InstanceType
pattern M5ad_2XLarge = InstanceType' "m5ad.2xlarge"

pattern M5ad_4XLarge :: InstanceType
pattern M5ad_4XLarge = InstanceType' "m5ad.4xlarge"

pattern M5ad_8XLarge :: InstanceType
pattern M5ad_8XLarge = InstanceType' "m5ad.8xlarge"

pattern M5ad_Large :: InstanceType
pattern M5ad_Large = InstanceType' "m5ad.large"

pattern M5ad_XLarge :: InstanceType
pattern M5ad_XLarge = InstanceType' "m5ad.xlarge"

pattern M5d_12XLarge :: InstanceType
pattern M5d_12XLarge = InstanceType' "m5d.12xlarge"

pattern M5d_16XLarge :: InstanceType
pattern M5d_16XLarge = InstanceType' "m5d.16xlarge"

pattern M5d_24XLarge :: InstanceType
pattern M5d_24XLarge = InstanceType' "m5d.24xlarge"

pattern M5d_2XLarge :: InstanceType
pattern M5d_2XLarge = InstanceType' "m5d.2xlarge"

pattern M5d_4XLarge :: InstanceType
pattern M5d_4XLarge = InstanceType' "m5d.4xlarge"

pattern M5d_8XLarge :: InstanceType
pattern M5d_8XLarge = InstanceType' "m5d.8xlarge"

pattern M5d_Large :: InstanceType
pattern M5d_Large = InstanceType' "m5d.large"

pattern M5d_Metal :: InstanceType
pattern M5d_Metal = InstanceType' "m5d.metal"

pattern M5d_XLarge :: InstanceType
pattern M5d_XLarge = InstanceType' "m5d.xlarge"

pattern M5dn_12XLarge :: InstanceType
pattern M5dn_12XLarge = InstanceType' "m5dn.12xlarge"

pattern M5dn_16XLarge :: InstanceType
pattern M5dn_16XLarge = InstanceType' "m5dn.16xlarge"

pattern M5dn_24XLarge :: InstanceType
pattern M5dn_24XLarge = InstanceType' "m5dn.24xlarge"

pattern M5dn_2XLarge :: InstanceType
pattern M5dn_2XLarge = InstanceType' "m5dn.2xlarge"

pattern M5dn_4XLarge :: InstanceType
pattern M5dn_4XLarge = InstanceType' "m5dn.4xlarge"

pattern M5dn_8XLarge :: InstanceType
pattern M5dn_8XLarge = InstanceType' "m5dn.8xlarge"

pattern M5dn_Large :: InstanceType
pattern M5dn_Large = InstanceType' "m5dn.large"

pattern M5dn_XLarge :: InstanceType
pattern M5dn_XLarge = InstanceType' "m5dn.xlarge"

pattern M5n_12XLarge :: InstanceType
pattern M5n_12XLarge = InstanceType' "m5n.12xlarge"

pattern M5n_16XLarge :: InstanceType
pattern M5n_16XLarge = InstanceType' "m5n.16xlarge"

pattern M5n_24XLarge :: InstanceType
pattern M5n_24XLarge = InstanceType' "m5n.24xlarge"

pattern M5n_2XLarge :: InstanceType
pattern M5n_2XLarge = InstanceType' "m5n.2xlarge"

pattern M5n_4XLarge :: InstanceType
pattern M5n_4XLarge = InstanceType' "m5n.4xlarge"

pattern M5n_8XLarge :: InstanceType
pattern M5n_8XLarge = InstanceType' "m5n.8xlarge"

pattern M5n_Large :: InstanceType
pattern M5n_Large = InstanceType' "m5n.large"

pattern M5n_XLarge :: InstanceType
pattern M5n_XLarge = InstanceType' "m5n.xlarge"

pattern M6g_12XLarge :: InstanceType
pattern M6g_12XLarge = InstanceType' "m6g.12xlarge"

pattern M6g_16XLarge :: InstanceType
pattern M6g_16XLarge = InstanceType' "m6g.16xlarge"

pattern M6g_2XLarge :: InstanceType
pattern M6g_2XLarge = InstanceType' "m6g.2xlarge"

pattern M6g_4XLarge :: InstanceType
pattern M6g_4XLarge = InstanceType' "m6g.4xlarge"

pattern M6g_8XLarge :: InstanceType
pattern M6g_8XLarge = InstanceType' "m6g.8xlarge"

pattern M6g_Large :: InstanceType
pattern M6g_Large = InstanceType' "m6g.large"

pattern M6g_Medium :: InstanceType
pattern M6g_Medium = InstanceType' "m6g.medium"

pattern M6g_Metal :: InstanceType
pattern M6g_Metal = InstanceType' "m6g.metal"

pattern M6g_XLarge :: InstanceType
pattern M6g_XLarge = InstanceType' "m6g.xlarge"

pattern M6gd_12XLarge :: InstanceType
pattern M6gd_12XLarge = InstanceType' "m6gd.12xlarge"

pattern M6gd_16XLarge :: InstanceType
pattern M6gd_16XLarge = InstanceType' "m6gd.16xlarge"

pattern M6gd_2XLarge :: InstanceType
pattern M6gd_2XLarge = InstanceType' "m6gd.2xlarge"

pattern M6gd_4XLarge :: InstanceType
pattern M6gd_4XLarge = InstanceType' "m6gd.4xlarge"

pattern M6gd_8XLarge :: InstanceType
pattern M6gd_8XLarge = InstanceType' "m6gd.8xlarge"

pattern M6gd_Large :: InstanceType
pattern M6gd_Large = InstanceType' "m6gd.large"

pattern M6gd_Medium :: InstanceType
pattern M6gd_Medium = InstanceType' "m6gd.medium"

pattern M6gd_Metal :: InstanceType
pattern M6gd_Metal = InstanceType' "m6gd.metal"

pattern M6gd_XLarge :: InstanceType
pattern M6gd_XLarge = InstanceType' "m6gd.xlarge"

pattern P2_16XLarge :: InstanceType
pattern P2_16XLarge = InstanceType' "p2.16xlarge"

pattern P2_8XLarge :: InstanceType
pattern P2_8XLarge = InstanceType' "p2.8xlarge"

pattern P2_XLarge :: InstanceType
pattern P2_XLarge = InstanceType' "p2.xlarge"

pattern P3_16XLarge :: InstanceType
pattern P3_16XLarge = InstanceType' "p3.16xlarge"

pattern P3_2XLarge :: InstanceType
pattern P3_2XLarge = InstanceType' "p3.2xlarge"

pattern P3_8XLarge :: InstanceType
pattern P3_8XLarge = InstanceType' "p3.8xlarge"

pattern P3dn_24XLarge :: InstanceType
pattern P3dn_24XLarge = InstanceType' "p3dn.24xlarge"

pattern P4d_24XLarge :: InstanceType
pattern P4d_24XLarge = InstanceType' "p4d.24xlarge"

pattern R3_2XLarge :: InstanceType
pattern R3_2XLarge = InstanceType' "r3.2xlarge"

pattern R3_4XLarge :: InstanceType
pattern R3_4XLarge = InstanceType' "r3.4xlarge"

pattern R3_8XLarge :: InstanceType
pattern R3_8XLarge = InstanceType' "r3.8xlarge"

pattern R3_Large :: InstanceType
pattern R3_Large = InstanceType' "r3.large"

pattern R3_XLarge :: InstanceType
pattern R3_XLarge = InstanceType' "r3.xlarge"

pattern R4_16XLarge :: InstanceType
pattern R4_16XLarge = InstanceType' "r4.16xlarge"

pattern R4_2XLarge :: InstanceType
pattern R4_2XLarge = InstanceType' "r4.2xlarge"

pattern R4_4XLarge :: InstanceType
pattern R4_4XLarge = InstanceType' "r4.4xlarge"

pattern R4_8XLarge :: InstanceType
pattern R4_8XLarge = InstanceType' "r4.8xlarge"

pattern R4_Large :: InstanceType
pattern R4_Large = InstanceType' "r4.large"

pattern R4_XLarge :: InstanceType
pattern R4_XLarge = InstanceType' "r4.xlarge"

pattern R5_12XLarge :: InstanceType
pattern R5_12XLarge = InstanceType' "r5.12xlarge"

pattern R5_16XLarge :: InstanceType
pattern R5_16XLarge = InstanceType' "r5.16xlarge"

pattern R5_24XLarge :: InstanceType
pattern R5_24XLarge = InstanceType' "r5.24xlarge"

pattern R5_2XLarge :: InstanceType
pattern R5_2XLarge = InstanceType' "r5.2xlarge"

pattern R5_4XLarge :: InstanceType
pattern R5_4XLarge = InstanceType' "r5.4xlarge"

pattern R5_8XLarge :: InstanceType
pattern R5_8XLarge = InstanceType' "r5.8xlarge"

pattern R5_Large :: InstanceType
pattern R5_Large = InstanceType' "r5.large"

pattern R5_Metal :: InstanceType
pattern R5_Metal = InstanceType' "r5.metal"

pattern R5_XLarge :: InstanceType
pattern R5_XLarge = InstanceType' "r5.xlarge"

pattern R5a_12XLarge :: InstanceType
pattern R5a_12XLarge = InstanceType' "r5a.12xlarge"

pattern R5a_16XLarge :: InstanceType
pattern R5a_16XLarge = InstanceType' "r5a.16xlarge"

pattern R5a_24XLarge :: InstanceType
pattern R5a_24XLarge = InstanceType' "r5a.24xlarge"

pattern R5a_2XLarge :: InstanceType
pattern R5a_2XLarge = InstanceType' "r5a.2xlarge"

pattern R5a_4XLarge :: InstanceType
pattern R5a_4XLarge = InstanceType' "r5a.4xlarge"

pattern R5a_8XLarge :: InstanceType
pattern R5a_8XLarge = InstanceType' "r5a.8xlarge"

pattern R5a_Large :: InstanceType
pattern R5a_Large = InstanceType' "r5a.large"

pattern R5a_XLarge :: InstanceType
pattern R5a_XLarge = InstanceType' "r5a.xlarge"

pattern R5ad_12XLarge :: InstanceType
pattern R5ad_12XLarge = InstanceType' "r5ad.12xlarge"

pattern R5ad_16XLarge :: InstanceType
pattern R5ad_16XLarge = InstanceType' "r5ad.16xlarge"

pattern R5ad_24XLarge :: InstanceType
pattern R5ad_24XLarge = InstanceType' "r5ad.24xlarge"

pattern R5ad_2XLarge :: InstanceType
pattern R5ad_2XLarge = InstanceType' "r5ad.2xlarge"

pattern R5ad_4XLarge :: InstanceType
pattern R5ad_4XLarge = InstanceType' "r5ad.4xlarge"

pattern R5ad_8XLarge :: InstanceType
pattern R5ad_8XLarge = InstanceType' "r5ad.8xlarge"

pattern R5ad_Large :: InstanceType
pattern R5ad_Large = InstanceType' "r5ad.large"

pattern R5ad_XLarge :: InstanceType
pattern R5ad_XLarge = InstanceType' "r5ad.xlarge"

pattern R5d_12XLarge :: InstanceType
pattern R5d_12XLarge = InstanceType' "r5d.12xlarge"

pattern R5d_16XLarge :: InstanceType
pattern R5d_16XLarge = InstanceType' "r5d.16xlarge"

pattern R5d_24XLarge :: InstanceType
pattern R5d_24XLarge = InstanceType' "r5d.24xlarge"

pattern R5d_2XLarge :: InstanceType
pattern R5d_2XLarge = InstanceType' "r5d.2xlarge"

pattern R5d_4XLarge :: InstanceType
pattern R5d_4XLarge = InstanceType' "r5d.4xlarge"

pattern R5d_8XLarge :: InstanceType
pattern R5d_8XLarge = InstanceType' "r5d.8xlarge"

pattern R5d_Large :: InstanceType
pattern R5d_Large = InstanceType' "r5d.large"

pattern R5d_Metal :: InstanceType
pattern R5d_Metal = InstanceType' "r5d.metal"

pattern R5d_XLarge :: InstanceType
pattern R5d_XLarge = InstanceType' "r5d.xlarge"

pattern R5dn_12XLarge :: InstanceType
pattern R5dn_12XLarge = InstanceType' "r5dn.12xlarge"

pattern R5dn_16XLarge :: InstanceType
pattern R5dn_16XLarge = InstanceType' "r5dn.16xlarge"

pattern R5dn_24XLarge :: InstanceType
pattern R5dn_24XLarge = InstanceType' "r5dn.24xlarge"

pattern R5dn_2XLarge :: InstanceType
pattern R5dn_2XLarge = InstanceType' "r5dn.2xlarge"

pattern R5dn_4XLarge :: InstanceType
pattern R5dn_4XLarge = InstanceType' "r5dn.4xlarge"

pattern R5dn_8XLarge :: InstanceType
pattern R5dn_8XLarge = InstanceType' "r5dn.8xlarge"

pattern R5dn_Large :: InstanceType
pattern R5dn_Large = InstanceType' "r5dn.large"

pattern R5dn_XLarge :: InstanceType
pattern R5dn_XLarge = InstanceType' "r5dn.xlarge"

pattern R5n_12XLarge :: InstanceType
pattern R5n_12XLarge = InstanceType' "r5n.12xlarge"

pattern R5n_16XLarge :: InstanceType
pattern R5n_16XLarge = InstanceType' "r5n.16xlarge"

pattern R5n_24XLarge :: InstanceType
pattern R5n_24XLarge = InstanceType' "r5n.24xlarge"

pattern R5n_2XLarge :: InstanceType
pattern R5n_2XLarge = InstanceType' "r5n.2xlarge"

pattern R5n_4XLarge :: InstanceType
pattern R5n_4XLarge = InstanceType' "r5n.4xlarge"

pattern R5n_8XLarge :: InstanceType
pattern R5n_8XLarge = InstanceType' "r5n.8xlarge"

pattern R5n_Large :: InstanceType
pattern R5n_Large = InstanceType' "r5n.large"

pattern R5n_XLarge :: InstanceType
pattern R5n_XLarge = InstanceType' "r5n.xlarge"

pattern R6g_12XLarge :: InstanceType
pattern R6g_12XLarge = InstanceType' "r6g.12xlarge"

pattern R6g_16XLarge :: InstanceType
pattern R6g_16XLarge = InstanceType' "r6g.16xlarge"

pattern R6g_2XLarge :: InstanceType
pattern R6g_2XLarge = InstanceType' "r6g.2xlarge"

pattern R6g_4XLarge :: InstanceType
pattern R6g_4XLarge = InstanceType' "r6g.4xlarge"

pattern R6g_8XLarge :: InstanceType
pattern R6g_8XLarge = InstanceType' "r6g.8xlarge"

pattern R6g_Large :: InstanceType
pattern R6g_Large = InstanceType' "r6g.large"

pattern R6g_Medium :: InstanceType
pattern R6g_Medium = InstanceType' "r6g.medium"

pattern R6g_Metal :: InstanceType
pattern R6g_Metal = InstanceType' "r6g.metal"

pattern R6g_XLarge :: InstanceType
pattern R6g_XLarge = InstanceType' "r6g.xlarge"

pattern R6gd_12XLarge :: InstanceType
pattern R6gd_12XLarge = InstanceType' "r6gd.12xlarge"

pattern R6gd_16XLarge :: InstanceType
pattern R6gd_16XLarge = InstanceType' "r6gd.16xlarge"

pattern R6gd_2XLarge :: InstanceType
pattern R6gd_2XLarge = InstanceType' "r6gd.2xlarge"

pattern R6gd_4XLarge :: InstanceType
pattern R6gd_4XLarge = InstanceType' "r6gd.4xlarge"

pattern R6gd_8XLarge :: InstanceType
pattern R6gd_8XLarge = InstanceType' "r6gd.8xlarge"

pattern R6gd_Large :: InstanceType
pattern R6gd_Large = InstanceType' "r6gd.large"

pattern R6gd_Medium :: InstanceType
pattern R6gd_Medium = InstanceType' "r6gd.medium"

pattern R6gd_Metal :: InstanceType
pattern R6gd_Metal = InstanceType' "r6gd.metal"

pattern R6gd_XLarge :: InstanceType
pattern R6gd_XLarge = InstanceType' "r6gd.xlarge"

pattern T1_Micro :: InstanceType
pattern T1_Micro = InstanceType' "t1.micro"

pattern T2_2XLarge :: InstanceType
pattern T2_2XLarge = InstanceType' "t2.2xlarge"

pattern T2_Large :: InstanceType
pattern T2_Large = InstanceType' "t2.large"

pattern T2_Medium :: InstanceType
pattern T2_Medium = InstanceType' "t2.medium"

pattern T2_Micro :: InstanceType
pattern T2_Micro = InstanceType' "t2.micro"

pattern T2_Nano :: InstanceType
pattern T2_Nano = InstanceType' "t2.nano"

pattern T2_Small :: InstanceType
pattern T2_Small = InstanceType' "t2.small"

pattern T2_XLarge :: InstanceType
pattern T2_XLarge = InstanceType' "t2.xlarge"

pattern T3_2XLarge :: InstanceType
pattern T3_2XLarge = InstanceType' "t3.2xlarge"

pattern T3_Large :: InstanceType
pattern T3_Large = InstanceType' "t3.large"

pattern T3_Medium :: InstanceType
pattern T3_Medium = InstanceType' "t3.medium"

pattern T3_Micro :: InstanceType
pattern T3_Micro = InstanceType' "t3.micro"

pattern T3_Nano :: InstanceType
pattern T3_Nano = InstanceType' "t3.nano"

pattern T3_Small :: InstanceType
pattern T3_Small = InstanceType' "t3.small"

pattern T3_XLarge :: InstanceType
pattern T3_XLarge = InstanceType' "t3.xlarge"

pattern T3a_2XLarge :: InstanceType
pattern T3a_2XLarge = InstanceType' "t3a.2xlarge"

pattern T3a_Large :: InstanceType
pattern T3a_Large = InstanceType' "t3a.large"

pattern T3a_Medium :: InstanceType
pattern T3a_Medium = InstanceType' "t3a.medium"

pattern T3a_Micro :: InstanceType
pattern T3a_Micro = InstanceType' "t3a.micro"

pattern T3a_Nano :: InstanceType
pattern T3a_Nano = InstanceType' "t3a.nano"

pattern T3a_Small :: InstanceType
pattern T3a_Small = InstanceType' "t3a.small"

pattern T3a_XLarge :: InstanceType
pattern T3a_XLarge = InstanceType' "t3a.xlarge"

pattern T4g_2XLarge :: InstanceType
pattern T4g_2XLarge = InstanceType' "t4g.2xlarge"

pattern T4g_Large :: InstanceType
pattern T4g_Large = InstanceType' "t4g.large"

pattern T4g_Medium :: InstanceType
pattern T4g_Medium = InstanceType' "t4g.medium"

pattern T4g_Micro :: InstanceType
pattern T4g_Micro = InstanceType' "t4g.micro"

pattern T4g_Nano :: InstanceType
pattern T4g_Nano = InstanceType' "t4g.nano"

pattern T4g_Small :: InstanceType
pattern T4g_Small = InstanceType' "t4g.small"

pattern T4g_XLarge :: InstanceType
pattern T4g_XLarge = InstanceType' "t4g.xlarge"

pattern U12TB1_Metal :: InstanceType
pattern U12TB1_Metal = InstanceType' "u-12tb1.metal"

pattern U18TB1_Metal :: InstanceType
pattern U18TB1_Metal = InstanceType' "u-18tb1.metal"

pattern U24TB1_Metal :: InstanceType
pattern U24TB1_Metal = InstanceType' "u-24tb1.metal"

pattern U6TB1_Metal :: InstanceType
pattern U6TB1_Metal = InstanceType' "u-6tb1.metal"

pattern U9TB1_Metal :: InstanceType
pattern U9TB1_Metal = InstanceType' "u-9tb1.metal"

pattern X1_16XLarge :: InstanceType
pattern X1_16XLarge = InstanceType' "x1.16xlarge"

pattern X1_32XLarge :: InstanceType
pattern X1_32XLarge = InstanceType' "x1.32xlarge"

pattern X1e_16XLarge :: InstanceType
pattern X1e_16XLarge = InstanceType' "x1e.16xlarge"

pattern X1e_2XLarge :: InstanceType
pattern X1e_2XLarge = InstanceType' "x1e.2xlarge"

pattern X1e_32XLarge :: InstanceType
pattern X1e_32XLarge = InstanceType' "x1e.32xlarge"

pattern X1e_4XLarge :: InstanceType
pattern X1e_4XLarge = InstanceType' "x1e.4xlarge"

pattern X1e_8XLarge :: InstanceType
pattern X1e_8XLarge = InstanceType' "x1e.8xlarge"

pattern X1e_XLarge :: InstanceType
pattern X1e_XLarge = InstanceType' "x1e.xlarge"

pattern Z1d_12XLarge :: InstanceType
pattern Z1d_12XLarge = InstanceType' "z1d.12xlarge"

pattern Z1d_2XLarge :: InstanceType
pattern Z1d_2XLarge = InstanceType' "z1d.2xlarge"

pattern Z1d_3XLarge :: InstanceType
pattern Z1d_3XLarge = InstanceType' "z1d.3xlarge"

pattern Z1d_6XLarge :: InstanceType
pattern Z1d_6XLarge = InstanceType' "z1d.6xlarge"

pattern Z1d_Large :: InstanceType
pattern Z1d_Large = InstanceType' "z1d.large"

pattern Z1d_Metal :: InstanceType
pattern Z1d_Metal = InstanceType' "z1d.metal"

pattern Z1d_XLarge :: InstanceType
pattern Z1d_XLarge = InstanceType' "z1d.xlarge"

{-# COMPLETE
  A1_2XLarge,
  A1_4XLarge,
  A1_Large,
  A1_Medium,
  A1_Metal,
  A1_XLarge,
  C1_Medium,
  C1_XLarge,
  C3_2XLarge,
  C3_4XLarge,
  C3_8XLarge,
  C3_Large,
  C3_XLarge,
  C4_2XLarge,
  C4_4XLarge,
  C4_8XLarge,
  C4_Large,
  C4_XLarge,
  C5_12XLarge,
  C5_18XLarge,
  C5_24XLarge,
  C5_2XLarge,
  C5_4XLarge,
  C5_9XLarge,
  C5_Large,
  C5_Metal,
  C5_XLarge,
  C5a_12XLarge,
  C5a_16XLarge,
  C5a_24XLarge,
  C5a_2XLarge,
  C5a_4XLarge,
  C5a_8XLarge,
  C5a_Large,
  C5a_XLarge,
  C5ad_12XLarge,
  C5ad_16XLarge,
  C5ad_24XLarge,
  C5ad_2XLarge,
  C5ad_4XLarge,
  C5ad_8XLarge,
  C5ad_Large,
  C5ad_XLarge,
  C5d_12XLarge,
  C5d_18XLarge,
  C5d_24XLarge,
  C5d_2XLarge,
  C5d_4XLarge,
  C5d_9XLarge,
  C5d_Large,
  C5d_Metal,
  C5d_XLarge,
  C5n_18XLarge,
  C5n_2XLarge,
  C5n_4XLarge,
  C5n_9XLarge,
  C5n_Large,
  C5n_XLarge,
  C6g_12XLarge,
  C6g_16XLarge,
  C6g_2XLarge,
  C6g_4XLarge,
  C6g_8XLarge,
  C6g_Large,
  C6g_Medium,
  C6g_Metal,
  C6g_XLarge,
  C6gd_12XLarge,
  C6gd_16XLarge,
  C6gd_2XLarge,
  C6gd_4XLarge,
  C6gd_8XLarge,
  C6gd_Large,
  C6gd_Medium,
  C6gd_Metal,
  C6gd_XLarge,
  CC1_4XLarge,
  CC2_8XLarge,
  CG1_4XLarge,
  CR1_8XLarge,
  D2_2XLarge,
  D2_4XLarge,
  D2_8XLarge,
  D2_XLarge,
  F1_16XLarge,
  F1_2XLarge,
  F1_4XLarge,
  G2_2XLarge,
  G2_8XLarge,
  G3_16XLarge,
  G3_4XLarge,
  G3_8XLarge,
  G3s_XLarge,
  G4dn_12XLarge,
  G4dn_16XLarge,
  G4dn_2XLarge,
  G4dn_4XLarge,
  G4dn_8XLarge,
  G4dn_Metal,
  G4dn_XLarge,
  H1_16XLarge,
  H1_2XLarge,
  H1_4XLarge,
  H1_8XLarge,
  HI1_4XLarge,
  HS1_8XLarge,
  I2_2XLarge,
  I2_4XLarge,
  I2_8XLarge,
  I2_XLarge,
  I3_16XLarge,
  I3_2XLarge,
  I3_4XLarge,
  I3_8XLarge,
  I3_Large,
  I3_Metal,
  I3_XLarge,
  I3en_12XLarge,
  I3en_24XLarge,
  I3en_2XLarge,
  I3en_3XLarge,
  I3en_6XLarge,
  I3en_Large,
  I3en_Metal,
  I3en_XLarge,
  INF1_24XLarge,
  INF1_2XLarge,
  INF1_6XLarge,
  INF1_XLarge,
  M1_Large,
  M1_Medium,
  M1_Small,
  M1_XLarge,
  M2_2XLarge,
  M2_4XLarge,
  M2_XLarge,
  M3_2XLarge,
  M3_Large,
  M3_Medium,
  M3_XLarge,
  M4_10XLarge,
  M4_16XLarge,
  M4_2XLarge,
  M4_4XLarge,
  M4_Large,
  M4_XLarge,
  M5_12XLarge,
  M5_16XLarge,
  M5_24XLarge,
  M5_2XLarge,
  M5_4XLarge,
  M5_8XLarge,
  M5_Large,
  M5_Metal,
  M5_XLarge,
  M5a_12XLarge,
  M5a_16XLarge,
  M5a_24XLarge,
  M5a_2XLarge,
  M5a_4XLarge,
  M5a_8XLarge,
  M5a_Large,
  M5a_XLarge,
  M5ad_12XLarge,
  M5ad_16XLarge,
  M5ad_24XLarge,
  M5ad_2XLarge,
  M5ad_4XLarge,
  M5ad_8XLarge,
  M5ad_Large,
  M5ad_XLarge,
  M5d_12XLarge,
  M5d_16XLarge,
  M5d_24XLarge,
  M5d_2XLarge,
  M5d_4XLarge,
  M5d_8XLarge,
  M5d_Large,
  M5d_Metal,
  M5d_XLarge,
  M5dn_12XLarge,
  M5dn_16XLarge,
  M5dn_24XLarge,
  M5dn_2XLarge,
  M5dn_4XLarge,
  M5dn_8XLarge,
  M5dn_Large,
  M5dn_XLarge,
  M5n_12XLarge,
  M5n_16XLarge,
  M5n_24XLarge,
  M5n_2XLarge,
  M5n_4XLarge,
  M5n_8XLarge,
  M5n_Large,
  M5n_XLarge,
  M6g_12XLarge,
  M6g_16XLarge,
  M6g_2XLarge,
  M6g_4XLarge,
  M6g_8XLarge,
  M6g_Large,
  M6g_Medium,
  M6g_Metal,
  M6g_XLarge,
  M6gd_12XLarge,
  M6gd_16XLarge,
  M6gd_2XLarge,
  M6gd_4XLarge,
  M6gd_8XLarge,
  M6gd_Large,
  M6gd_Medium,
  M6gd_Metal,
  M6gd_XLarge,
  P2_16XLarge,
  P2_8XLarge,
  P2_XLarge,
  P3_16XLarge,
  P3_2XLarge,
  P3_8XLarge,
  P3dn_24XLarge,
  P4d_24XLarge,
  R3_2XLarge,
  R3_4XLarge,
  R3_8XLarge,
  R3_Large,
  R3_XLarge,
  R4_16XLarge,
  R4_2XLarge,
  R4_4XLarge,
  R4_8XLarge,
  R4_Large,
  R4_XLarge,
  R5_12XLarge,
  R5_16XLarge,
  R5_24XLarge,
  R5_2XLarge,
  R5_4XLarge,
  R5_8XLarge,
  R5_Large,
  R5_Metal,
  R5_XLarge,
  R5a_12XLarge,
  R5a_16XLarge,
  R5a_24XLarge,
  R5a_2XLarge,
  R5a_4XLarge,
  R5a_8XLarge,
  R5a_Large,
  R5a_XLarge,
  R5ad_12XLarge,
  R5ad_16XLarge,
  R5ad_24XLarge,
  R5ad_2XLarge,
  R5ad_4XLarge,
  R5ad_8XLarge,
  R5ad_Large,
  R5ad_XLarge,
  R5d_12XLarge,
  R5d_16XLarge,
  R5d_24XLarge,
  R5d_2XLarge,
  R5d_4XLarge,
  R5d_8XLarge,
  R5d_Large,
  R5d_Metal,
  R5d_XLarge,
  R5dn_12XLarge,
  R5dn_16XLarge,
  R5dn_24XLarge,
  R5dn_2XLarge,
  R5dn_4XLarge,
  R5dn_8XLarge,
  R5dn_Large,
  R5dn_XLarge,
  R5n_12XLarge,
  R5n_16XLarge,
  R5n_24XLarge,
  R5n_2XLarge,
  R5n_4XLarge,
  R5n_8XLarge,
  R5n_Large,
  R5n_XLarge,
  R6g_12XLarge,
  R6g_16XLarge,
  R6g_2XLarge,
  R6g_4XLarge,
  R6g_8XLarge,
  R6g_Large,
  R6g_Medium,
  R6g_Metal,
  R6g_XLarge,
  R6gd_12XLarge,
  R6gd_16XLarge,
  R6gd_2XLarge,
  R6gd_4XLarge,
  R6gd_8XLarge,
  R6gd_Large,
  R6gd_Medium,
  R6gd_Metal,
  R6gd_XLarge,
  T1_Micro,
  T2_2XLarge,
  T2_Large,
  T2_Medium,
  T2_Micro,
  T2_Nano,
  T2_Small,
  T2_XLarge,
  T3_2XLarge,
  T3_Large,
  T3_Medium,
  T3_Micro,
  T3_Nano,
  T3_Small,
  T3_XLarge,
  T3a_2XLarge,
  T3a_Large,
  T3a_Medium,
  T3a_Micro,
  T3a_Nano,
  T3a_Small,
  T3a_XLarge,
  T4g_2XLarge,
  T4g_Large,
  T4g_Medium,
  T4g_Micro,
  T4g_Nano,
  T4g_Small,
  T4g_XLarge,
  U12TB1_Metal,
  U18TB1_Metal,
  U24TB1_Metal,
  U6TB1_Metal,
  U9TB1_Metal,
  X1_16XLarge,
  X1_32XLarge,
  X1e_16XLarge,
  X1e_2XLarge,
  X1e_32XLarge,
  X1e_4XLarge,
  X1e_8XLarge,
  X1e_XLarge,
  Z1d_12XLarge,
  Z1d_2XLarge,
  Z1d_3XLarge,
  Z1d_6XLarge,
  Z1d_Large,
  Z1d_Metal,
  Z1d_XLarge,
  InstanceType'
  #-}
