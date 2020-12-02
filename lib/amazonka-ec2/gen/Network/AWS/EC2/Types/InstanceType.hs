{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceType
  = A1_2XLarge
  | A1_4XLarge
  | A1_Large
  | A1_Medium
  | A1_Metal
  | A1_XLarge
  | C1_Medium
  | C1_XLarge
  | C3_2XLarge
  | C3_4XLarge
  | C3_8XLarge
  | C3_Large
  | C3_XLarge
  | C4_2XLarge
  | C4_4XLarge
  | C4_8XLarge
  | C4_Large
  | C4_XLarge
  | C5_12XLarge
  | C5_18XLarge
  | C5_24XLarge
  | C5_2XLarge
  | C5_4XLarge
  | C5_9XLarge
  | C5_Large
  | C5_Metal
  | C5_XLarge
  | C5a_12XLarge
  | C5a_16XLarge
  | C5a_24XLarge
  | C5a_2XLarge
  | C5a_4XLarge
  | C5a_8XLarge
  | C5a_Large
  | C5a_XLarge
  | C5ad_12XLarge
  | C5ad_16XLarge
  | C5ad_24XLarge
  | C5ad_2XLarge
  | C5ad_4XLarge
  | C5ad_8XLarge
  | C5ad_Large
  | C5ad_XLarge
  | C5d_12XLarge
  | C5d_18XLarge
  | C5d_24XLarge
  | C5d_2XLarge
  | C5d_4XLarge
  | C5d_9XLarge
  | C5d_Large
  | C5d_Metal
  | C5d_XLarge
  | C5n_18XLarge
  | C5n_2XLarge
  | C5n_4XLarge
  | C5n_9XLarge
  | C5n_Large
  | C5n_XLarge
  | C6g_12XLarge
  | C6g_16XLarge
  | C6g_2XLarge
  | C6g_4XLarge
  | C6g_8XLarge
  | C6g_Large
  | C6g_Medium
  | C6g_Metal
  | C6g_XLarge
  | C6gd_12XLarge
  | C6gd_16XLarge
  | C6gd_2XLarge
  | C6gd_4XLarge
  | C6gd_8XLarge
  | C6gd_Large
  | C6gd_Medium
  | C6gd_Metal
  | C6gd_XLarge
  | CC1_4XLarge
  | CC2_8XLarge
  | CG1_4XLarge
  | CR1_8XLarge
  | D2_2XLarge
  | D2_4XLarge
  | D2_8XLarge
  | D2_XLarge
  | F1_16XLarge
  | F1_2XLarge
  | F1_4XLarge
  | G2_2XLarge
  | G2_8XLarge
  | G3_16XLarge
  | G3_4XLarge
  | G3_8XLarge
  | G3s_XLarge
  | G4dn_12XLarge
  | G4dn_16XLarge
  | G4dn_2XLarge
  | G4dn_4XLarge
  | G4dn_8XLarge
  | G4dn_Metal
  | G4dn_XLarge
  | H1_16XLarge
  | H1_2XLarge
  | H1_4XLarge
  | H1_8XLarge
  | HI1_4XLarge
  | HS1_8XLarge
  | I2_2XLarge
  | I2_4XLarge
  | I2_8XLarge
  | I2_XLarge
  | I3_16XLarge
  | I3_2XLarge
  | I3_4XLarge
  | I3_8XLarge
  | I3_Large
  | I3_Metal
  | I3_XLarge
  | I3en_12XLarge
  | I3en_24XLarge
  | I3en_2XLarge
  | I3en_3XLarge
  | I3en_6XLarge
  | I3en_Large
  | I3en_Metal
  | I3en_XLarge
  | INF1_24XLarge
  | INF1_2XLarge
  | INF1_6XLarge
  | INF1_XLarge
  | M1_Large
  | M1_Medium
  | M1_Small
  | M1_XLarge
  | M2_2XLarge
  | M2_4XLarge
  | M2_XLarge
  | M3_2XLarge
  | M3_Large
  | M3_Medium
  | M3_XLarge
  | M4_10XLarge
  | M4_16XLarge
  | M4_2XLarge
  | M4_4XLarge
  | M4_Large
  | M4_XLarge
  | M5_12XLarge
  | M5_16XLarge
  | M5_24XLarge
  | M5_2XLarge
  | M5_4XLarge
  | M5_8XLarge
  | M5_Large
  | M5_Metal
  | M5_XLarge
  | M5a_12XLarge
  | M5a_16XLarge
  | M5a_24XLarge
  | M5a_2XLarge
  | M5a_4XLarge
  | M5a_8XLarge
  | M5a_Large
  | M5a_XLarge
  | M5ad_12XLarge
  | M5ad_16XLarge
  | M5ad_24XLarge
  | M5ad_2XLarge
  | M5ad_4XLarge
  | M5ad_8XLarge
  | M5ad_Large
  | M5ad_XLarge
  | M5d_12XLarge
  | M5d_16XLarge
  | M5d_24XLarge
  | M5d_2XLarge
  | M5d_4XLarge
  | M5d_8XLarge
  | M5d_Large
  | M5d_Metal
  | M5d_XLarge
  | M5dn_12XLarge
  | M5dn_16XLarge
  | M5dn_24XLarge
  | M5dn_2XLarge
  | M5dn_4XLarge
  | M5dn_8XLarge
  | M5dn_Large
  | M5dn_XLarge
  | M5n_12XLarge
  | M5n_16XLarge
  | M5n_24XLarge
  | M5n_2XLarge
  | M5n_4XLarge
  | M5n_8XLarge
  | M5n_Large
  | M5n_XLarge
  | M6g_12XLarge
  | M6g_16XLarge
  | M6g_2XLarge
  | M6g_4XLarge
  | M6g_8XLarge
  | M6g_Large
  | M6g_Medium
  | M6g_Metal
  | M6g_XLarge
  | M6gd_12XLarge
  | M6gd_16XLarge
  | M6gd_2XLarge
  | M6gd_4XLarge
  | M6gd_8XLarge
  | M6gd_Large
  | M6gd_Medium
  | M6gd_Metal
  | M6gd_XLarge
  | P2_16XLarge
  | P2_8XLarge
  | P2_XLarge
  | P3_16XLarge
  | P3_2XLarge
  | P3_8XLarge
  | P3dn_24XLarge
  | P4d_24XLarge
  | R3_2XLarge
  | R3_4XLarge
  | R3_8XLarge
  | R3_Large
  | R3_XLarge
  | R4_16XLarge
  | R4_2XLarge
  | R4_4XLarge
  | R4_8XLarge
  | R4_Large
  | R4_XLarge
  | R5_12XLarge
  | R5_16XLarge
  | R5_24XLarge
  | R5_2XLarge
  | R5_4XLarge
  | R5_8XLarge
  | R5_Large
  | R5_Metal
  | R5_XLarge
  | R5a_12XLarge
  | R5a_16XLarge
  | R5a_24XLarge
  | R5a_2XLarge
  | R5a_4XLarge
  | R5a_8XLarge
  | R5a_Large
  | R5a_XLarge
  | R5ad_12XLarge
  | R5ad_16XLarge
  | R5ad_24XLarge
  | R5ad_2XLarge
  | R5ad_4XLarge
  | R5ad_8XLarge
  | R5ad_Large
  | R5ad_XLarge
  | R5d_12XLarge
  | R5d_16XLarge
  | R5d_24XLarge
  | R5d_2XLarge
  | R5d_4XLarge
  | R5d_8XLarge
  | R5d_Large
  | R5d_Metal
  | R5d_XLarge
  | R5dn_12XLarge
  | R5dn_16XLarge
  | R5dn_24XLarge
  | R5dn_2XLarge
  | R5dn_4XLarge
  | R5dn_8XLarge
  | R5dn_Large
  | R5dn_XLarge
  | R5n_12XLarge
  | R5n_16XLarge
  | R5n_24XLarge
  | R5n_2XLarge
  | R5n_4XLarge
  | R5n_8XLarge
  | R5n_Large
  | R5n_XLarge
  | R6g_12XLarge
  | R6g_16XLarge
  | R6g_2XLarge
  | R6g_4XLarge
  | R6g_8XLarge
  | R6g_Large
  | R6g_Medium
  | R6g_Metal
  | R6g_XLarge
  | R6gd_12XLarge
  | R6gd_16XLarge
  | R6gd_2XLarge
  | R6gd_4XLarge
  | R6gd_8XLarge
  | R6gd_Large
  | R6gd_Medium
  | R6gd_Metal
  | R6gd_XLarge
  | T1_Micro
  | T2_2XLarge
  | T2_Large
  | T2_Medium
  | T2_Micro
  | T2_Nano
  | T2_Small
  | T2_XLarge
  | T3_2XLarge
  | T3_Large
  | T3_Medium
  | T3_Micro
  | T3_Nano
  | T3_Small
  | T3_XLarge
  | T3a_2XLarge
  | T3a_Large
  | T3a_Medium
  | T3a_Micro
  | T3a_Nano
  | T3a_Small
  | T3a_XLarge
  | T4g_2XLarge
  | T4g_Large
  | T4g_Medium
  | T4g_Micro
  | T4g_Nano
  | T4g_Small
  | T4g_XLarge
  | U12TB1_Metal
  | U18TB1_Metal
  | U24TB1_Metal
  | U6TB1_Metal
  | U9TB1_Metal
  | X1_16XLarge
  | X1_32XLarge
  | X1e_16XLarge
  | X1e_2XLarge
  | X1e_32XLarge
  | X1e_4XLarge
  | X1e_8XLarge
  | X1e_XLarge
  | Z1d_12XLarge
  | Z1d_2XLarge
  | Z1d_3XLarge
  | Z1d_6XLarge
  | Z1d_Large
  | Z1d_Metal
  | Z1d_XLarge
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText InstanceType where
  parser =
    takeLowerText >>= \case
      "a1.2xlarge" -> pure A1_2XLarge
      "a1.4xlarge" -> pure A1_4XLarge
      "a1.large" -> pure A1_Large
      "a1.medium" -> pure A1_Medium
      "a1.metal" -> pure A1_Metal
      "a1.xlarge" -> pure A1_XLarge
      "c1.medium" -> pure C1_Medium
      "c1.xlarge" -> pure C1_XLarge
      "c3.2xlarge" -> pure C3_2XLarge
      "c3.4xlarge" -> pure C3_4XLarge
      "c3.8xlarge" -> pure C3_8XLarge
      "c3.large" -> pure C3_Large
      "c3.xlarge" -> pure C3_XLarge
      "c4.2xlarge" -> pure C4_2XLarge
      "c4.4xlarge" -> pure C4_4XLarge
      "c4.8xlarge" -> pure C4_8XLarge
      "c4.large" -> pure C4_Large
      "c4.xlarge" -> pure C4_XLarge
      "c5.12xlarge" -> pure C5_12XLarge
      "c5.18xlarge" -> pure C5_18XLarge
      "c5.24xlarge" -> pure C5_24XLarge
      "c5.2xlarge" -> pure C5_2XLarge
      "c5.4xlarge" -> pure C5_4XLarge
      "c5.9xlarge" -> pure C5_9XLarge
      "c5.large" -> pure C5_Large
      "c5.metal" -> pure C5_Metal
      "c5.xlarge" -> pure C5_XLarge
      "c5a.12xlarge" -> pure C5a_12XLarge
      "c5a.16xlarge" -> pure C5a_16XLarge
      "c5a.24xlarge" -> pure C5a_24XLarge
      "c5a.2xlarge" -> pure C5a_2XLarge
      "c5a.4xlarge" -> pure C5a_4XLarge
      "c5a.8xlarge" -> pure C5a_8XLarge
      "c5a.large" -> pure C5a_Large
      "c5a.xlarge" -> pure C5a_XLarge
      "c5ad.12xlarge" -> pure C5ad_12XLarge
      "c5ad.16xlarge" -> pure C5ad_16XLarge
      "c5ad.24xlarge" -> pure C5ad_24XLarge
      "c5ad.2xlarge" -> pure C5ad_2XLarge
      "c5ad.4xlarge" -> pure C5ad_4XLarge
      "c5ad.8xlarge" -> pure C5ad_8XLarge
      "c5ad.large" -> pure C5ad_Large
      "c5ad.xlarge" -> pure C5ad_XLarge
      "c5d.12xlarge" -> pure C5d_12XLarge
      "c5d.18xlarge" -> pure C5d_18XLarge
      "c5d.24xlarge" -> pure C5d_24XLarge
      "c5d.2xlarge" -> pure C5d_2XLarge
      "c5d.4xlarge" -> pure C5d_4XLarge
      "c5d.9xlarge" -> pure C5d_9XLarge
      "c5d.large" -> pure C5d_Large
      "c5d.metal" -> pure C5d_Metal
      "c5d.xlarge" -> pure C5d_XLarge
      "c5n.18xlarge" -> pure C5n_18XLarge
      "c5n.2xlarge" -> pure C5n_2XLarge
      "c5n.4xlarge" -> pure C5n_4XLarge
      "c5n.9xlarge" -> pure C5n_9XLarge
      "c5n.large" -> pure C5n_Large
      "c5n.xlarge" -> pure C5n_XLarge
      "c6g.12xlarge" -> pure C6g_12XLarge
      "c6g.16xlarge" -> pure C6g_16XLarge
      "c6g.2xlarge" -> pure C6g_2XLarge
      "c6g.4xlarge" -> pure C6g_4XLarge
      "c6g.8xlarge" -> pure C6g_8XLarge
      "c6g.large" -> pure C6g_Large
      "c6g.medium" -> pure C6g_Medium
      "c6g.metal" -> pure C6g_Metal
      "c6g.xlarge" -> pure C6g_XLarge
      "c6gd.12xlarge" -> pure C6gd_12XLarge
      "c6gd.16xlarge" -> pure C6gd_16XLarge
      "c6gd.2xlarge" -> pure C6gd_2XLarge
      "c6gd.4xlarge" -> pure C6gd_4XLarge
      "c6gd.8xlarge" -> pure C6gd_8XLarge
      "c6gd.large" -> pure C6gd_Large
      "c6gd.medium" -> pure C6gd_Medium
      "c6gd.metal" -> pure C6gd_Metal
      "c6gd.xlarge" -> pure C6gd_XLarge
      "cc1.4xlarge" -> pure CC1_4XLarge
      "cc2.8xlarge" -> pure CC2_8XLarge
      "cg1.4xlarge" -> pure CG1_4XLarge
      "cr1.8xlarge" -> pure CR1_8XLarge
      "d2.2xlarge" -> pure D2_2XLarge
      "d2.4xlarge" -> pure D2_4XLarge
      "d2.8xlarge" -> pure D2_8XLarge
      "d2.xlarge" -> pure D2_XLarge
      "f1.16xlarge" -> pure F1_16XLarge
      "f1.2xlarge" -> pure F1_2XLarge
      "f1.4xlarge" -> pure F1_4XLarge
      "g2.2xlarge" -> pure G2_2XLarge
      "g2.8xlarge" -> pure G2_8XLarge
      "g3.16xlarge" -> pure G3_16XLarge
      "g3.4xlarge" -> pure G3_4XLarge
      "g3.8xlarge" -> pure G3_8XLarge
      "g3s.xlarge" -> pure G3s_XLarge
      "g4dn.12xlarge" -> pure G4dn_12XLarge
      "g4dn.16xlarge" -> pure G4dn_16XLarge
      "g4dn.2xlarge" -> pure G4dn_2XLarge
      "g4dn.4xlarge" -> pure G4dn_4XLarge
      "g4dn.8xlarge" -> pure G4dn_8XLarge
      "g4dn.metal" -> pure G4dn_Metal
      "g4dn.xlarge" -> pure G4dn_XLarge
      "h1.16xlarge" -> pure H1_16XLarge
      "h1.2xlarge" -> pure H1_2XLarge
      "h1.4xlarge" -> pure H1_4XLarge
      "h1.8xlarge" -> pure H1_8XLarge
      "hi1.4xlarge" -> pure HI1_4XLarge
      "hs1.8xlarge" -> pure HS1_8XLarge
      "i2.2xlarge" -> pure I2_2XLarge
      "i2.4xlarge" -> pure I2_4XLarge
      "i2.8xlarge" -> pure I2_8XLarge
      "i2.xlarge" -> pure I2_XLarge
      "i3.16xlarge" -> pure I3_16XLarge
      "i3.2xlarge" -> pure I3_2XLarge
      "i3.4xlarge" -> pure I3_4XLarge
      "i3.8xlarge" -> pure I3_8XLarge
      "i3.large" -> pure I3_Large
      "i3.metal" -> pure I3_Metal
      "i3.xlarge" -> pure I3_XLarge
      "i3en.12xlarge" -> pure I3en_12XLarge
      "i3en.24xlarge" -> pure I3en_24XLarge
      "i3en.2xlarge" -> pure I3en_2XLarge
      "i3en.3xlarge" -> pure I3en_3XLarge
      "i3en.6xlarge" -> pure I3en_6XLarge
      "i3en.large" -> pure I3en_Large
      "i3en.metal" -> pure I3en_Metal
      "i3en.xlarge" -> pure I3en_XLarge
      "inf1.24xlarge" -> pure INF1_24XLarge
      "inf1.2xlarge" -> pure INF1_2XLarge
      "inf1.6xlarge" -> pure INF1_6XLarge
      "inf1.xlarge" -> pure INF1_XLarge
      "m1.large" -> pure M1_Large
      "m1.medium" -> pure M1_Medium
      "m1.small" -> pure M1_Small
      "m1.xlarge" -> pure M1_XLarge
      "m2.2xlarge" -> pure M2_2XLarge
      "m2.4xlarge" -> pure M2_4XLarge
      "m2.xlarge" -> pure M2_XLarge
      "m3.2xlarge" -> pure M3_2XLarge
      "m3.large" -> pure M3_Large
      "m3.medium" -> pure M3_Medium
      "m3.xlarge" -> pure M3_XLarge
      "m4.10xlarge" -> pure M4_10XLarge
      "m4.16xlarge" -> pure M4_16XLarge
      "m4.2xlarge" -> pure M4_2XLarge
      "m4.4xlarge" -> pure M4_4XLarge
      "m4.large" -> pure M4_Large
      "m4.xlarge" -> pure M4_XLarge
      "m5.12xlarge" -> pure M5_12XLarge
      "m5.16xlarge" -> pure M5_16XLarge
      "m5.24xlarge" -> pure M5_24XLarge
      "m5.2xlarge" -> pure M5_2XLarge
      "m5.4xlarge" -> pure M5_4XLarge
      "m5.8xlarge" -> pure M5_8XLarge
      "m5.large" -> pure M5_Large
      "m5.metal" -> pure M5_Metal
      "m5.xlarge" -> pure M5_XLarge
      "m5a.12xlarge" -> pure M5a_12XLarge
      "m5a.16xlarge" -> pure M5a_16XLarge
      "m5a.24xlarge" -> pure M5a_24XLarge
      "m5a.2xlarge" -> pure M5a_2XLarge
      "m5a.4xlarge" -> pure M5a_4XLarge
      "m5a.8xlarge" -> pure M5a_8XLarge
      "m5a.large" -> pure M5a_Large
      "m5a.xlarge" -> pure M5a_XLarge
      "m5ad.12xlarge" -> pure M5ad_12XLarge
      "m5ad.16xlarge" -> pure M5ad_16XLarge
      "m5ad.24xlarge" -> pure M5ad_24XLarge
      "m5ad.2xlarge" -> pure M5ad_2XLarge
      "m5ad.4xlarge" -> pure M5ad_4XLarge
      "m5ad.8xlarge" -> pure M5ad_8XLarge
      "m5ad.large" -> pure M5ad_Large
      "m5ad.xlarge" -> pure M5ad_XLarge
      "m5d.12xlarge" -> pure M5d_12XLarge
      "m5d.16xlarge" -> pure M5d_16XLarge
      "m5d.24xlarge" -> pure M5d_24XLarge
      "m5d.2xlarge" -> pure M5d_2XLarge
      "m5d.4xlarge" -> pure M5d_4XLarge
      "m5d.8xlarge" -> pure M5d_8XLarge
      "m5d.large" -> pure M5d_Large
      "m5d.metal" -> pure M5d_Metal
      "m5d.xlarge" -> pure M5d_XLarge
      "m5dn.12xlarge" -> pure M5dn_12XLarge
      "m5dn.16xlarge" -> pure M5dn_16XLarge
      "m5dn.24xlarge" -> pure M5dn_24XLarge
      "m5dn.2xlarge" -> pure M5dn_2XLarge
      "m5dn.4xlarge" -> pure M5dn_4XLarge
      "m5dn.8xlarge" -> pure M5dn_8XLarge
      "m5dn.large" -> pure M5dn_Large
      "m5dn.xlarge" -> pure M5dn_XLarge
      "m5n.12xlarge" -> pure M5n_12XLarge
      "m5n.16xlarge" -> pure M5n_16XLarge
      "m5n.24xlarge" -> pure M5n_24XLarge
      "m5n.2xlarge" -> pure M5n_2XLarge
      "m5n.4xlarge" -> pure M5n_4XLarge
      "m5n.8xlarge" -> pure M5n_8XLarge
      "m5n.large" -> pure M5n_Large
      "m5n.xlarge" -> pure M5n_XLarge
      "m6g.12xlarge" -> pure M6g_12XLarge
      "m6g.16xlarge" -> pure M6g_16XLarge
      "m6g.2xlarge" -> pure M6g_2XLarge
      "m6g.4xlarge" -> pure M6g_4XLarge
      "m6g.8xlarge" -> pure M6g_8XLarge
      "m6g.large" -> pure M6g_Large
      "m6g.medium" -> pure M6g_Medium
      "m6g.metal" -> pure M6g_Metal
      "m6g.xlarge" -> pure M6g_XLarge
      "m6gd.12xlarge" -> pure M6gd_12XLarge
      "m6gd.16xlarge" -> pure M6gd_16XLarge
      "m6gd.2xlarge" -> pure M6gd_2XLarge
      "m6gd.4xlarge" -> pure M6gd_4XLarge
      "m6gd.8xlarge" -> pure M6gd_8XLarge
      "m6gd.large" -> pure M6gd_Large
      "m6gd.medium" -> pure M6gd_Medium
      "m6gd.metal" -> pure M6gd_Metal
      "m6gd.xlarge" -> pure M6gd_XLarge
      "p2.16xlarge" -> pure P2_16XLarge
      "p2.8xlarge" -> pure P2_8XLarge
      "p2.xlarge" -> pure P2_XLarge
      "p3.16xlarge" -> pure P3_16XLarge
      "p3.2xlarge" -> pure P3_2XLarge
      "p3.8xlarge" -> pure P3_8XLarge
      "p3dn.24xlarge" -> pure P3dn_24XLarge
      "p4d.24xlarge" -> pure P4d_24XLarge
      "r3.2xlarge" -> pure R3_2XLarge
      "r3.4xlarge" -> pure R3_4XLarge
      "r3.8xlarge" -> pure R3_8XLarge
      "r3.large" -> pure R3_Large
      "r3.xlarge" -> pure R3_XLarge
      "r4.16xlarge" -> pure R4_16XLarge
      "r4.2xlarge" -> pure R4_2XLarge
      "r4.4xlarge" -> pure R4_4XLarge
      "r4.8xlarge" -> pure R4_8XLarge
      "r4.large" -> pure R4_Large
      "r4.xlarge" -> pure R4_XLarge
      "r5.12xlarge" -> pure R5_12XLarge
      "r5.16xlarge" -> pure R5_16XLarge
      "r5.24xlarge" -> pure R5_24XLarge
      "r5.2xlarge" -> pure R5_2XLarge
      "r5.4xlarge" -> pure R5_4XLarge
      "r5.8xlarge" -> pure R5_8XLarge
      "r5.large" -> pure R5_Large
      "r5.metal" -> pure R5_Metal
      "r5.xlarge" -> pure R5_XLarge
      "r5a.12xlarge" -> pure R5a_12XLarge
      "r5a.16xlarge" -> pure R5a_16XLarge
      "r5a.24xlarge" -> pure R5a_24XLarge
      "r5a.2xlarge" -> pure R5a_2XLarge
      "r5a.4xlarge" -> pure R5a_4XLarge
      "r5a.8xlarge" -> pure R5a_8XLarge
      "r5a.large" -> pure R5a_Large
      "r5a.xlarge" -> pure R5a_XLarge
      "r5ad.12xlarge" -> pure R5ad_12XLarge
      "r5ad.16xlarge" -> pure R5ad_16XLarge
      "r5ad.24xlarge" -> pure R5ad_24XLarge
      "r5ad.2xlarge" -> pure R5ad_2XLarge
      "r5ad.4xlarge" -> pure R5ad_4XLarge
      "r5ad.8xlarge" -> pure R5ad_8XLarge
      "r5ad.large" -> pure R5ad_Large
      "r5ad.xlarge" -> pure R5ad_XLarge
      "r5d.12xlarge" -> pure R5d_12XLarge
      "r5d.16xlarge" -> pure R5d_16XLarge
      "r5d.24xlarge" -> pure R5d_24XLarge
      "r5d.2xlarge" -> pure R5d_2XLarge
      "r5d.4xlarge" -> pure R5d_4XLarge
      "r5d.8xlarge" -> pure R5d_8XLarge
      "r5d.large" -> pure R5d_Large
      "r5d.metal" -> pure R5d_Metal
      "r5d.xlarge" -> pure R5d_XLarge
      "r5dn.12xlarge" -> pure R5dn_12XLarge
      "r5dn.16xlarge" -> pure R5dn_16XLarge
      "r5dn.24xlarge" -> pure R5dn_24XLarge
      "r5dn.2xlarge" -> pure R5dn_2XLarge
      "r5dn.4xlarge" -> pure R5dn_4XLarge
      "r5dn.8xlarge" -> pure R5dn_8XLarge
      "r5dn.large" -> pure R5dn_Large
      "r5dn.xlarge" -> pure R5dn_XLarge
      "r5n.12xlarge" -> pure R5n_12XLarge
      "r5n.16xlarge" -> pure R5n_16XLarge
      "r5n.24xlarge" -> pure R5n_24XLarge
      "r5n.2xlarge" -> pure R5n_2XLarge
      "r5n.4xlarge" -> pure R5n_4XLarge
      "r5n.8xlarge" -> pure R5n_8XLarge
      "r5n.large" -> pure R5n_Large
      "r5n.xlarge" -> pure R5n_XLarge
      "r6g.12xlarge" -> pure R6g_12XLarge
      "r6g.16xlarge" -> pure R6g_16XLarge
      "r6g.2xlarge" -> pure R6g_2XLarge
      "r6g.4xlarge" -> pure R6g_4XLarge
      "r6g.8xlarge" -> pure R6g_8XLarge
      "r6g.large" -> pure R6g_Large
      "r6g.medium" -> pure R6g_Medium
      "r6g.metal" -> pure R6g_Metal
      "r6g.xlarge" -> pure R6g_XLarge
      "r6gd.12xlarge" -> pure R6gd_12XLarge
      "r6gd.16xlarge" -> pure R6gd_16XLarge
      "r6gd.2xlarge" -> pure R6gd_2XLarge
      "r6gd.4xlarge" -> pure R6gd_4XLarge
      "r6gd.8xlarge" -> pure R6gd_8XLarge
      "r6gd.large" -> pure R6gd_Large
      "r6gd.medium" -> pure R6gd_Medium
      "r6gd.metal" -> pure R6gd_Metal
      "r6gd.xlarge" -> pure R6gd_XLarge
      "t1.micro" -> pure T1_Micro
      "t2.2xlarge" -> pure T2_2XLarge
      "t2.large" -> pure T2_Large
      "t2.medium" -> pure T2_Medium
      "t2.micro" -> pure T2_Micro
      "t2.nano" -> pure T2_Nano
      "t2.small" -> pure T2_Small
      "t2.xlarge" -> pure T2_XLarge
      "t3.2xlarge" -> pure T3_2XLarge
      "t3.large" -> pure T3_Large
      "t3.medium" -> pure T3_Medium
      "t3.micro" -> pure T3_Micro
      "t3.nano" -> pure T3_Nano
      "t3.small" -> pure T3_Small
      "t3.xlarge" -> pure T3_XLarge
      "t3a.2xlarge" -> pure T3a_2XLarge
      "t3a.large" -> pure T3a_Large
      "t3a.medium" -> pure T3a_Medium
      "t3a.micro" -> pure T3a_Micro
      "t3a.nano" -> pure T3a_Nano
      "t3a.small" -> pure T3a_Small
      "t3a.xlarge" -> pure T3a_XLarge
      "t4g.2xlarge" -> pure T4g_2XLarge
      "t4g.large" -> pure T4g_Large
      "t4g.medium" -> pure T4g_Medium
      "t4g.micro" -> pure T4g_Micro
      "t4g.nano" -> pure T4g_Nano
      "t4g.small" -> pure T4g_Small
      "t4g.xlarge" -> pure T4g_XLarge
      "u-12tb1.metal" -> pure U12TB1_Metal
      "u-18tb1.metal" -> pure U18TB1_Metal
      "u-24tb1.metal" -> pure U24TB1_Metal
      "u-6tb1.metal" -> pure U6TB1_Metal
      "u-9tb1.metal" -> pure U9TB1_Metal
      "x1.16xlarge" -> pure X1_16XLarge
      "x1.32xlarge" -> pure X1_32XLarge
      "x1e.16xlarge" -> pure X1e_16XLarge
      "x1e.2xlarge" -> pure X1e_2XLarge
      "x1e.32xlarge" -> pure X1e_32XLarge
      "x1e.4xlarge" -> pure X1e_4XLarge
      "x1e.8xlarge" -> pure X1e_8XLarge
      "x1e.xlarge" -> pure X1e_XLarge
      "z1d.12xlarge" -> pure Z1d_12XLarge
      "z1d.2xlarge" -> pure Z1d_2XLarge
      "z1d.3xlarge" -> pure Z1d_3XLarge
      "z1d.6xlarge" -> pure Z1d_6XLarge
      "z1d.large" -> pure Z1d_Large
      "z1d.metal" -> pure Z1d_Metal
      "z1d.xlarge" -> pure Z1d_XLarge
      e ->
        fromTextError $
          "Failure parsing InstanceType from value: '" <> e
            <> "'. Accepted values: a1.2xlarge, a1.4xlarge, a1.large, a1.medium, a1.metal, a1.xlarge, c1.medium, c1.xlarge, c3.2xlarge, c3.4xlarge, c3.8xlarge, c3.large, c3.xlarge, c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, c5.12xlarge, c5.18xlarge, c5.24xlarge, c5.2xlarge, c5.4xlarge, c5.9xlarge, c5.large, c5.metal, c5.xlarge, c5a.12xlarge, c5a.16xlarge, c5a.24xlarge, c5a.2xlarge, c5a.4xlarge, c5a.8xlarge, c5a.large, c5a.xlarge, c5ad.12xlarge, c5ad.16xlarge, c5ad.24xlarge, c5ad.2xlarge, c5ad.4xlarge, c5ad.8xlarge, c5ad.large, c5ad.xlarge, c5d.12xlarge, c5d.18xlarge, c5d.24xlarge, c5d.2xlarge, c5d.4xlarge, c5d.9xlarge, c5d.large, c5d.metal, c5d.xlarge, c5n.18xlarge, c5n.2xlarge, c5n.4xlarge, c5n.9xlarge, c5n.large, c5n.xlarge, c6g.12xlarge, c6g.16xlarge, c6g.2xlarge, c6g.4xlarge, c6g.8xlarge, c6g.large, c6g.medium, c6g.metal, c6g.xlarge, c6gd.12xlarge, c6gd.16xlarge, c6gd.2xlarge, c6gd.4xlarge, c6gd.8xlarge, c6gd.large, c6gd.medium, c6gd.metal, c6gd.xlarge, cc1.4xlarge, cc2.8xlarge, cg1.4xlarge, cr1.8xlarge, d2.2xlarge, d2.4xlarge, d2.8xlarge, d2.xlarge, f1.16xlarge, f1.2xlarge, f1.4xlarge, g2.2xlarge, g2.8xlarge, g3.16xlarge, g3.4xlarge, g3.8xlarge, g3s.xlarge, g4dn.12xlarge, g4dn.16xlarge, g4dn.2xlarge, g4dn.4xlarge, g4dn.8xlarge, g4dn.metal, g4dn.xlarge, h1.16xlarge, h1.2xlarge, h1.4xlarge, h1.8xlarge, hi1.4xlarge, hs1.8xlarge, i2.2xlarge, i2.4xlarge, i2.8xlarge, i2.xlarge, i3.16xlarge, i3.2xlarge, i3.4xlarge, i3.8xlarge, i3.large, i3.metal, i3.xlarge, i3en.12xlarge, i3en.24xlarge, i3en.2xlarge, i3en.3xlarge, i3en.6xlarge, i3en.large, i3en.metal, i3en.xlarge, inf1.24xlarge, inf1.2xlarge, inf1.6xlarge, inf1.xlarge, m1.large, m1.medium, m1.small, m1.xlarge, m2.2xlarge, m2.4xlarge, m2.xlarge, m3.2xlarge, m3.large, m3.medium, m3.xlarge, m4.10xlarge, m4.16xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, m5.12xlarge, m5.16xlarge, m5.24xlarge, m5.2xlarge, m5.4xlarge, m5.8xlarge, m5.large, m5.metal, m5.xlarge, m5a.12xlarge, m5a.16xlarge, m5a.24xlarge, m5a.2xlarge, m5a.4xlarge, m5a.8xlarge, m5a.large, m5a.xlarge, m5ad.12xlarge, m5ad.16xlarge, m5ad.24xlarge, m5ad.2xlarge, m5ad.4xlarge, m5ad.8xlarge, m5ad.large, m5ad.xlarge, m5d.12xlarge, m5d.16xlarge, m5d.24xlarge, m5d.2xlarge, m5d.4xlarge, m5d.8xlarge, m5d.large, m5d.metal, m5d.xlarge, m5dn.12xlarge, m5dn.16xlarge, m5dn.24xlarge, m5dn.2xlarge, m5dn.4xlarge, m5dn.8xlarge, m5dn.large, m5dn.xlarge, m5n.12xlarge, m5n.16xlarge, m5n.24xlarge, m5n.2xlarge, m5n.4xlarge, m5n.8xlarge, m5n.large, m5n.xlarge, m6g.12xlarge, m6g.16xlarge, m6g.2xlarge, m6g.4xlarge, m6g.8xlarge, m6g.large, m6g.medium, m6g.metal, m6g.xlarge, m6gd.12xlarge, m6gd.16xlarge, m6gd.2xlarge, m6gd.4xlarge, m6gd.8xlarge, m6gd.large, m6gd.medium, m6gd.metal, m6gd.xlarge, p2.16xlarge, p2.8xlarge, p2.xlarge, p3.16xlarge, p3.2xlarge, p3.8xlarge, p3dn.24xlarge, p4d.24xlarge, r3.2xlarge, r3.4xlarge, r3.8xlarge, r3.large, r3.xlarge, r4.16xlarge, r4.2xlarge, r4.4xlarge, r4.8xlarge, r4.large, r4.xlarge, r5.12xlarge, r5.16xlarge, r5.24xlarge, r5.2xlarge, r5.4xlarge, r5.8xlarge, r5.large, r5.metal, r5.xlarge, r5a.12xlarge, r5a.16xlarge, r5a.24xlarge, r5a.2xlarge, r5a.4xlarge, r5a.8xlarge, r5a.large, r5a.xlarge, r5ad.12xlarge, r5ad.16xlarge, r5ad.24xlarge, r5ad.2xlarge, r5ad.4xlarge, r5ad.8xlarge, r5ad.large, r5ad.xlarge, r5d.12xlarge, r5d.16xlarge, r5d.24xlarge, r5d.2xlarge, r5d.4xlarge, r5d.8xlarge, r5d.large, r5d.metal, r5d.xlarge, r5dn.12xlarge, r5dn.16xlarge, r5dn.24xlarge, r5dn.2xlarge, r5dn.4xlarge, r5dn.8xlarge, r5dn.large, r5dn.xlarge, r5n.12xlarge, r5n.16xlarge, r5n.24xlarge, r5n.2xlarge, r5n.4xlarge, r5n.8xlarge, r5n.large, r5n.xlarge, r6g.12xlarge, r6g.16xlarge, r6g.2xlarge, r6g.4xlarge, r6g.8xlarge, r6g.large, r6g.medium, r6g.metal, r6g.xlarge, r6gd.12xlarge, r6gd.16xlarge, r6gd.2xlarge, r6gd.4xlarge, r6gd.8xlarge, r6gd.large, r6gd.medium, r6gd.metal, r6gd.xlarge, t1.micro, t2.2xlarge, t2.large, t2.medium, t2.micro, t2.nano, t2.small, t2.xlarge, t3.2xlarge, t3.large, t3.medium, t3.micro, t3.nano, t3.small, t3.xlarge, t3a.2xlarge, t3a.large, t3a.medium, t3a.micro, t3a.nano, t3a.small, t3a.xlarge, t4g.2xlarge, t4g.large, t4g.medium, t4g.micro, t4g.nano, t4g.small, t4g.xlarge, u-12tb1.metal, u-18tb1.metal, u-24tb1.metal, u-6tb1.metal, u-9tb1.metal, x1.16xlarge, x1.32xlarge, x1e.16xlarge, x1e.2xlarge, x1e.32xlarge, x1e.4xlarge, x1e.8xlarge, x1e.xlarge, z1d.12xlarge, z1d.2xlarge, z1d.3xlarge, z1d.6xlarge, z1d.large, z1d.metal, z1d.xlarge"

instance ToText InstanceType where
  toText = \case
    A1_2XLarge -> "a1.2xlarge"
    A1_4XLarge -> "a1.4xlarge"
    A1_Large -> "a1.large"
    A1_Medium -> "a1.medium"
    A1_Metal -> "a1.metal"
    A1_XLarge -> "a1.xlarge"
    C1_Medium -> "c1.medium"
    C1_XLarge -> "c1.xlarge"
    C3_2XLarge -> "c3.2xlarge"
    C3_4XLarge -> "c3.4xlarge"
    C3_8XLarge -> "c3.8xlarge"
    C3_Large -> "c3.large"
    C3_XLarge -> "c3.xlarge"
    C4_2XLarge -> "c4.2xlarge"
    C4_4XLarge -> "c4.4xlarge"
    C4_8XLarge -> "c4.8xlarge"
    C4_Large -> "c4.large"
    C4_XLarge -> "c4.xlarge"
    C5_12XLarge -> "c5.12xlarge"
    C5_18XLarge -> "c5.18xlarge"
    C5_24XLarge -> "c5.24xlarge"
    C5_2XLarge -> "c5.2xlarge"
    C5_4XLarge -> "c5.4xlarge"
    C5_9XLarge -> "c5.9xlarge"
    C5_Large -> "c5.large"
    C5_Metal -> "c5.metal"
    C5_XLarge -> "c5.xlarge"
    C5a_12XLarge -> "c5a.12xlarge"
    C5a_16XLarge -> "c5a.16xlarge"
    C5a_24XLarge -> "c5a.24xlarge"
    C5a_2XLarge -> "c5a.2xlarge"
    C5a_4XLarge -> "c5a.4xlarge"
    C5a_8XLarge -> "c5a.8xlarge"
    C5a_Large -> "c5a.large"
    C5a_XLarge -> "c5a.xlarge"
    C5ad_12XLarge -> "c5ad.12xlarge"
    C5ad_16XLarge -> "c5ad.16xlarge"
    C5ad_24XLarge -> "c5ad.24xlarge"
    C5ad_2XLarge -> "c5ad.2xlarge"
    C5ad_4XLarge -> "c5ad.4xlarge"
    C5ad_8XLarge -> "c5ad.8xlarge"
    C5ad_Large -> "c5ad.large"
    C5ad_XLarge -> "c5ad.xlarge"
    C5d_12XLarge -> "c5d.12xlarge"
    C5d_18XLarge -> "c5d.18xlarge"
    C5d_24XLarge -> "c5d.24xlarge"
    C5d_2XLarge -> "c5d.2xlarge"
    C5d_4XLarge -> "c5d.4xlarge"
    C5d_9XLarge -> "c5d.9xlarge"
    C5d_Large -> "c5d.large"
    C5d_Metal -> "c5d.metal"
    C5d_XLarge -> "c5d.xlarge"
    C5n_18XLarge -> "c5n.18xlarge"
    C5n_2XLarge -> "c5n.2xlarge"
    C5n_4XLarge -> "c5n.4xlarge"
    C5n_9XLarge -> "c5n.9xlarge"
    C5n_Large -> "c5n.large"
    C5n_XLarge -> "c5n.xlarge"
    C6g_12XLarge -> "c6g.12xlarge"
    C6g_16XLarge -> "c6g.16xlarge"
    C6g_2XLarge -> "c6g.2xlarge"
    C6g_4XLarge -> "c6g.4xlarge"
    C6g_8XLarge -> "c6g.8xlarge"
    C6g_Large -> "c6g.large"
    C6g_Medium -> "c6g.medium"
    C6g_Metal -> "c6g.metal"
    C6g_XLarge -> "c6g.xlarge"
    C6gd_12XLarge -> "c6gd.12xlarge"
    C6gd_16XLarge -> "c6gd.16xlarge"
    C6gd_2XLarge -> "c6gd.2xlarge"
    C6gd_4XLarge -> "c6gd.4xlarge"
    C6gd_8XLarge -> "c6gd.8xlarge"
    C6gd_Large -> "c6gd.large"
    C6gd_Medium -> "c6gd.medium"
    C6gd_Metal -> "c6gd.metal"
    C6gd_XLarge -> "c6gd.xlarge"
    CC1_4XLarge -> "cc1.4xlarge"
    CC2_8XLarge -> "cc2.8xlarge"
    CG1_4XLarge -> "cg1.4xlarge"
    CR1_8XLarge -> "cr1.8xlarge"
    D2_2XLarge -> "d2.2xlarge"
    D2_4XLarge -> "d2.4xlarge"
    D2_8XLarge -> "d2.8xlarge"
    D2_XLarge -> "d2.xlarge"
    F1_16XLarge -> "f1.16xlarge"
    F1_2XLarge -> "f1.2xlarge"
    F1_4XLarge -> "f1.4xlarge"
    G2_2XLarge -> "g2.2xlarge"
    G2_8XLarge -> "g2.8xlarge"
    G3_16XLarge -> "g3.16xlarge"
    G3_4XLarge -> "g3.4xlarge"
    G3_8XLarge -> "g3.8xlarge"
    G3s_XLarge -> "g3s.xlarge"
    G4dn_12XLarge -> "g4dn.12xlarge"
    G4dn_16XLarge -> "g4dn.16xlarge"
    G4dn_2XLarge -> "g4dn.2xlarge"
    G4dn_4XLarge -> "g4dn.4xlarge"
    G4dn_8XLarge -> "g4dn.8xlarge"
    G4dn_Metal -> "g4dn.metal"
    G4dn_XLarge -> "g4dn.xlarge"
    H1_16XLarge -> "h1.16xlarge"
    H1_2XLarge -> "h1.2xlarge"
    H1_4XLarge -> "h1.4xlarge"
    H1_8XLarge -> "h1.8xlarge"
    HI1_4XLarge -> "hi1.4xlarge"
    HS1_8XLarge -> "hs1.8xlarge"
    I2_2XLarge -> "i2.2xlarge"
    I2_4XLarge -> "i2.4xlarge"
    I2_8XLarge -> "i2.8xlarge"
    I2_XLarge -> "i2.xlarge"
    I3_16XLarge -> "i3.16xlarge"
    I3_2XLarge -> "i3.2xlarge"
    I3_4XLarge -> "i3.4xlarge"
    I3_8XLarge -> "i3.8xlarge"
    I3_Large -> "i3.large"
    I3_Metal -> "i3.metal"
    I3_XLarge -> "i3.xlarge"
    I3en_12XLarge -> "i3en.12xlarge"
    I3en_24XLarge -> "i3en.24xlarge"
    I3en_2XLarge -> "i3en.2xlarge"
    I3en_3XLarge -> "i3en.3xlarge"
    I3en_6XLarge -> "i3en.6xlarge"
    I3en_Large -> "i3en.large"
    I3en_Metal -> "i3en.metal"
    I3en_XLarge -> "i3en.xlarge"
    INF1_24XLarge -> "inf1.24xlarge"
    INF1_2XLarge -> "inf1.2xlarge"
    INF1_6XLarge -> "inf1.6xlarge"
    INF1_XLarge -> "inf1.xlarge"
    M1_Large -> "m1.large"
    M1_Medium -> "m1.medium"
    M1_Small -> "m1.small"
    M1_XLarge -> "m1.xlarge"
    M2_2XLarge -> "m2.2xlarge"
    M2_4XLarge -> "m2.4xlarge"
    M2_XLarge -> "m2.xlarge"
    M3_2XLarge -> "m3.2xlarge"
    M3_Large -> "m3.large"
    M3_Medium -> "m3.medium"
    M3_XLarge -> "m3.xlarge"
    M4_10XLarge -> "m4.10xlarge"
    M4_16XLarge -> "m4.16xlarge"
    M4_2XLarge -> "m4.2xlarge"
    M4_4XLarge -> "m4.4xlarge"
    M4_Large -> "m4.large"
    M4_XLarge -> "m4.xlarge"
    M5_12XLarge -> "m5.12xlarge"
    M5_16XLarge -> "m5.16xlarge"
    M5_24XLarge -> "m5.24xlarge"
    M5_2XLarge -> "m5.2xlarge"
    M5_4XLarge -> "m5.4xlarge"
    M5_8XLarge -> "m5.8xlarge"
    M5_Large -> "m5.large"
    M5_Metal -> "m5.metal"
    M5_XLarge -> "m5.xlarge"
    M5a_12XLarge -> "m5a.12xlarge"
    M5a_16XLarge -> "m5a.16xlarge"
    M5a_24XLarge -> "m5a.24xlarge"
    M5a_2XLarge -> "m5a.2xlarge"
    M5a_4XLarge -> "m5a.4xlarge"
    M5a_8XLarge -> "m5a.8xlarge"
    M5a_Large -> "m5a.large"
    M5a_XLarge -> "m5a.xlarge"
    M5ad_12XLarge -> "m5ad.12xlarge"
    M5ad_16XLarge -> "m5ad.16xlarge"
    M5ad_24XLarge -> "m5ad.24xlarge"
    M5ad_2XLarge -> "m5ad.2xlarge"
    M5ad_4XLarge -> "m5ad.4xlarge"
    M5ad_8XLarge -> "m5ad.8xlarge"
    M5ad_Large -> "m5ad.large"
    M5ad_XLarge -> "m5ad.xlarge"
    M5d_12XLarge -> "m5d.12xlarge"
    M5d_16XLarge -> "m5d.16xlarge"
    M5d_24XLarge -> "m5d.24xlarge"
    M5d_2XLarge -> "m5d.2xlarge"
    M5d_4XLarge -> "m5d.4xlarge"
    M5d_8XLarge -> "m5d.8xlarge"
    M5d_Large -> "m5d.large"
    M5d_Metal -> "m5d.metal"
    M5d_XLarge -> "m5d.xlarge"
    M5dn_12XLarge -> "m5dn.12xlarge"
    M5dn_16XLarge -> "m5dn.16xlarge"
    M5dn_24XLarge -> "m5dn.24xlarge"
    M5dn_2XLarge -> "m5dn.2xlarge"
    M5dn_4XLarge -> "m5dn.4xlarge"
    M5dn_8XLarge -> "m5dn.8xlarge"
    M5dn_Large -> "m5dn.large"
    M5dn_XLarge -> "m5dn.xlarge"
    M5n_12XLarge -> "m5n.12xlarge"
    M5n_16XLarge -> "m5n.16xlarge"
    M5n_24XLarge -> "m5n.24xlarge"
    M5n_2XLarge -> "m5n.2xlarge"
    M5n_4XLarge -> "m5n.4xlarge"
    M5n_8XLarge -> "m5n.8xlarge"
    M5n_Large -> "m5n.large"
    M5n_XLarge -> "m5n.xlarge"
    M6g_12XLarge -> "m6g.12xlarge"
    M6g_16XLarge -> "m6g.16xlarge"
    M6g_2XLarge -> "m6g.2xlarge"
    M6g_4XLarge -> "m6g.4xlarge"
    M6g_8XLarge -> "m6g.8xlarge"
    M6g_Large -> "m6g.large"
    M6g_Medium -> "m6g.medium"
    M6g_Metal -> "m6g.metal"
    M6g_XLarge -> "m6g.xlarge"
    M6gd_12XLarge -> "m6gd.12xlarge"
    M6gd_16XLarge -> "m6gd.16xlarge"
    M6gd_2XLarge -> "m6gd.2xlarge"
    M6gd_4XLarge -> "m6gd.4xlarge"
    M6gd_8XLarge -> "m6gd.8xlarge"
    M6gd_Large -> "m6gd.large"
    M6gd_Medium -> "m6gd.medium"
    M6gd_Metal -> "m6gd.metal"
    M6gd_XLarge -> "m6gd.xlarge"
    P2_16XLarge -> "p2.16xlarge"
    P2_8XLarge -> "p2.8xlarge"
    P2_XLarge -> "p2.xlarge"
    P3_16XLarge -> "p3.16xlarge"
    P3_2XLarge -> "p3.2xlarge"
    P3_8XLarge -> "p3.8xlarge"
    P3dn_24XLarge -> "p3dn.24xlarge"
    P4d_24XLarge -> "p4d.24xlarge"
    R3_2XLarge -> "r3.2xlarge"
    R3_4XLarge -> "r3.4xlarge"
    R3_8XLarge -> "r3.8xlarge"
    R3_Large -> "r3.large"
    R3_XLarge -> "r3.xlarge"
    R4_16XLarge -> "r4.16xlarge"
    R4_2XLarge -> "r4.2xlarge"
    R4_4XLarge -> "r4.4xlarge"
    R4_8XLarge -> "r4.8xlarge"
    R4_Large -> "r4.large"
    R4_XLarge -> "r4.xlarge"
    R5_12XLarge -> "r5.12xlarge"
    R5_16XLarge -> "r5.16xlarge"
    R5_24XLarge -> "r5.24xlarge"
    R5_2XLarge -> "r5.2xlarge"
    R5_4XLarge -> "r5.4xlarge"
    R5_8XLarge -> "r5.8xlarge"
    R5_Large -> "r5.large"
    R5_Metal -> "r5.metal"
    R5_XLarge -> "r5.xlarge"
    R5a_12XLarge -> "r5a.12xlarge"
    R5a_16XLarge -> "r5a.16xlarge"
    R5a_24XLarge -> "r5a.24xlarge"
    R5a_2XLarge -> "r5a.2xlarge"
    R5a_4XLarge -> "r5a.4xlarge"
    R5a_8XLarge -> "r5a.8xlarge"
    R5a_Large -> "r5a.large"
    R5a_XLarge -> "r5a.xlarge"
    R5ad_12XLarge -> "r5ad.12xlarge"
    R5ad_16XLarge -> "r5ad.16xlarge"
    R5ad_24XLarge -> "r5ad.24xlarge"
    R5ad_2XLarge -> "r5ad.2xlarge"
    R5ad_4XLarge -> "r5ad.4xlarge"
    R5ad_8XLarge -> "r5ad.8xlarge"
    R5ad_Large -> "r5ad.large"
    R5ad_XLarge -> "r5ad.xlarge"
    R5d_12XLarge -> "r5d.12xlarge"
    R5d_16XLarge -> "r5d.16xlarge"
    R5d_24XLarge -> "r5d.24xlarge"
    R5d_2XLarge -> "r5d.2xlarge"
    R5d_4XLarge -> "r5d.4xlarge"
    R5d_8XLarge -> "r5d.8xlarge"
    R5d_Large -> "r5d.large"
    R5d_Metal -> "r5d.metal"
    R5d_XLarge -> "r5d.xlarge"
    R5dn_12XLarge -> "r5dn.12xlarge"
    R5dn_16XLarge -> "r5dn.16xlarge"
    R5dn_24XLarge -> "r5dn.24xlarge"
    R5dn_2XLarge -> "r5dn.2xlarge"
    R5dn_4XLarge -> "r5dn.4xlarge"
    R5dn_8XLarge -> "r5dn.8xlarge"
    R5dn_Large -> "r5dn.large"
    R5dn_XLarge -> "r5dn.xlarge"
    R5n_12XLarge -> "r5n.12xlarge"
    R5n_16XLarge -> "r5n.16xlarge"
    R5n_24XLarge -> "r5n.24xlarge"
    R5n_2XLarge -> "r5n.2xlarge"
    R5n_4XLarge -> "r5n.4xlarge"
    R5n_8XLarge -> "r5n.8xlarge"
    R5n_Large -> "r5n.large"
    R5n_XLarge -> "r5n.xlarge"
    R6g_12XLarge -> "r6g.12xlarge"
    R6g_16XLarge -> "r6g.16xlarge"
    R6g_2XLarge -> "r6g.2xlarge"
    R6g_4XLarge -> "r6g.4xlarge"
    R6g_8XLarge -> "r6g.8xlarge"
    R6g_Large -> "r6g.large"
    R6g_Medium -> "r6g.medium"
    R6g_Metal -> "r6g.metal"
    R6g_XLarge -> "r6g.xlarge"
    R6gd_12XLarge -> "r6gd.12xlarge"
    R6gd_16XLarge -> "r6gd.16xlarge"
    R6gd_2XLarge -> "r6gd.2xlarge"
    R6gd_4XLarge -> "r6gd.4xlarge"
    R6gd_8XLarge -> "r6gd.8xlarge"
    R6gd_Large -> "r6gd.large"
    R6gd_Medium -> "r6gd.medium"
    R6gd_Metal -> "r6gd.metal"
    R6gd_XLarge -> "r6gd.xlarge"
    T1_Micro -> "t1.micro"
    T2_2XLarge -> "t2.2xlarge"
    T2_Large -> "t2.large"
    T2_Medium -> "t2.medium"
    T2_Micro -> "t2.micro"
    T2_Nano -> "t2.nano"
    T2_Small -> "t2.small"
    T2_XLarge -> "t2.xlarge"
    T3_2XLarge -> "t3.2xlarge"
    T3_Large -> "t3.large"
    T3_Medium -> "t3.medium"
    T3_Micro -> "t3.micro"
    T3_Nano -> "t3.nano"
    T3_Small -> "t3.small"
    T3_XLarge -> "t3.xlarge"
    T3a_2XLarge -> "t3a.2xlarge"
    T3a_Large -> "t3a.large"
    T3a_Medium -> "t3a.medium"
    T3a_Micro -> "t3a.micro"
    T3a_Nano -> "t3a.nano"
    T3a_Small -> "t3a.small"
    T3a_XLarge -> "t3a.xlarge"
    T4g_2XLarge -> "t4g.2xlarge"
    T4g_Large -> "t4g.large"
    T4g_Medium -> "t4g.medium"
    T4g_Micro -> "t4g.micro"
    T4g_Nano -> "t4g.nano"
    T4g_Small -> "t4g.small"
    T4g_XLarge -> "t4g.xlarge"
    U12TB1_Metal -> "u-12tb1.metal"
    U18TB1_Metal -> "u-18tb1.metal"
    U24TB1_Metal -> "u-24tb1.metal"
    U6TB1_Metal -> "u-6tb1.metal"
    U9TB1_Metal -> "u-9tb1.metal"
    X1_16XLarge -> "x1.16xlarge"
    X1_32XLarge -> "x1.32xlarge"
    X1e_16XLarge -> "x1e.16xlarge"
    X1e_2XLarge -> "x1e.2xlarge"
    X1e_32XLarge -> "x1e.32xlarge"
    X1e_4XLarge -> "x1e.4xlarge"
    X1e_8XLarge -> "x1e.8xlarge"
    X1e_XLarge -> "x1e.xlarge"
    Z1d_12XLarge -> "z1d.12xlarge"
    Z1d_2XLarge -> "z1d.2xlarge"
    Z1d_3XLarge -> "z1d.3xlarge"
    Z1d_6XLarge -> "z1d.6xlarge"
    Z1d_Large -> "z1d.large"
    Z1d_Metal -> "z1d.metal"
    Z1d_XLarge -> "z1d.xlarge"

instance Hashable InstanceType

instance NFData InstanceType

instance ToByteString InstanceType

instance ToQuery InstanceType

instance ToHeader InstanceType

instance FromXML InstanceType where
  parseXML = parseXMLText "InstanceType"
