{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceIpConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceIpConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>).
-- Used to create an allow list of IP addresses for a private workforce.
-- Workers will only be able to login to their worker portal from an IP
-- address within this range. By default, a workforce isn\'t restricted to
-- specific IP addresses.
--
-- /See:/ 'newSourceIpConfig' smart constructor.
data SourceIpConfig = SourceIpConfig'
  { -- | A list of one to ten
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing>
    -- (CIDR) values.
    --
    -- Maximum: Ten CIDR values
    --
    -- The following Length Constraints apply to individual CIDR values in the
    -- CIDR value list.
    cidrs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SourceIpConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrs', 'sourceIpConfig_cidrs' - A list of one to ten
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing>
-- (CIDR) values.
--
-- Maximum: Ten CIDR values
--
-- The following Length Constraints apply to individual CIDR values in the
-- CIDR value list.
newSourceIpConfig ::
  SourceIpConfig
newSourceIpConfig =
  SourceIpConfig' {cidrs = Prelude.mempty}

-- | A list of one to ten
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing>
-- (CIDR) values.
--
-- Maximum: Ten CIDR values
--
-- The following Length Constraints apply to individual CIDR values in the
-- CIDR value list.
sourceIpConfig_cidrs :: Lens.Lens' SourceIpConfig [Prelude.Text]
sourceIpConfig_cidrs = Lens.lens (\SourceIpConfig' {cidrs} -> cidrs) (\s@SourceIpConfig' {} a -> s {cidrs = a} :: SourceIpConfig) Prelude.. Prelude._Coerce

instance Prelude.FromJSON SourceIpConfig where
  parseJSON =
    Prelude.withObject
      "SourceIpConfig"
      ( \x ->
          SourceIpConfig'
            Prelude.<$> (x Prelude..:? "Cidrs" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable SourceIpConfig

instance Prelude.NFData SourceIpConfig

instance Prelude.ToJSON SourceIpConfig where
  toJSON SourceIpConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Cidrs" Prelude..= cidrs)]
      )
