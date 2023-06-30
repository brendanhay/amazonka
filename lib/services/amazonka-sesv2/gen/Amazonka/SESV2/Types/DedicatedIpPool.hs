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
-- Module      : Amazonka.SESV2.Types.DedicatedIpPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DedicatedIpPool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ScalingMode

-- | Contains information about a dedicated IP pool.
--
-- /See:/ 'newDedicatedIpPool' smart constructor.
data DedicatedIpPool = DedicatedIpPool'
  { -- | The name of the dedicated IP pool.
    poolName :: Prelude.Text,
    -- | The type of the dedicated IP pool.
    --
    -- -   @STANDARD@ – A dedicated IP pool where the customer can control
    --     which IPs are part of the pool.
    --
    -- -   @MANAGED@ – A dedicated IP pool where the reputation and number of
    --     IPs is automatically managed by Amazon SES.
    scalingMode :: ScalingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DedicatedIpPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolName', 'dedicatedIpPool_poolName' - The name of the dedicated IP pool.
--
-- 'scalingMode', 'dedicatedIpPool_scalingMode' - The type of the dedicated IP pool.
--
-- -   @STANDARD@ – A dedicated IP pool where the customer can control
--     which IPs are part of the pool.
--
-- -   @MANAGED@ – A dedicated IP pool where the reputation and number of
--     IPs is automatically managed by Amazon SES.
newDedicatedIpPool ::
  -- | 'poolName'
  Prelude.Text ->
  -- | 'scalingMode'
  ScalingMode ->
  DedicatedIpPool
newDedicatedIpPool pPoolName_ pScalingMode_ =
  DedicatedIpPool'
    { poolName = pPoolName_,
      scalingMode = pScalingMode_
    }

-- | The name of the dedicated IP pool.
dedicatedIpPool_poolName :: Lens.Lens' DedicatedIpPool Prelude.Text
dedicatedIpPool_poolName = Lens.lens (\DedicatedIpPool' {poolName} -> poolName) (\s@DedicatedIpPool' {} a -> s {poolName = a} :: DedicatedIpPool)

-- | The type of the dedicated IP pool.
--
-- -   @STANDARD@ – A dedicated IP pool where the customer can control
--     which IPs are part of the pool.
--
-- -   @MANAGED@ – A dedicated IP pool where the reputation and number of
--     IPs is automatically managed by Amazon SES.
dedicatedIpPool_scalingMode :: Lens.Lens' DedicatedIpPool ScalingMode
dedicatedIpPool_scalingMode = Lens.lens (\DedicatedIpPool' {scalingMode} -> scalingMode) (\s@DedicatedIpPool' {} a -> s {scalingMode = a} :: DedicatedIpPool)

instance Data.FromJSON DedicatedIpPool where
  parseJSON =
    Data.withObject
      "DedicatedIpPool"
      ( \x ->
          DedicatedIpPool'
            Prelude.<$> (x Data..: "PoolName")
            Prelude.<*> (x Data..: "ScalingMode")
      )

instance Prelude.Hashable DedicatedIpPool where
  hashWithSalt _salt DedicatedIpPool' {..} =
    _salt
      `Prelude.hashWithSalt` poolName
      `Prelude.hashWithSalt` scalingMode

instance Prelude.NFData DedicatedIpPool where
  rnf DedicatedIpPool' {..} =
    Prelude.rnf poolName
      `Prelude.seq` Prelude.rnf scalingMode
