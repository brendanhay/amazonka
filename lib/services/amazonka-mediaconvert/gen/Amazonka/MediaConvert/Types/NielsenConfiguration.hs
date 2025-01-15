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
-- Module      : Amazonka.MediaConvert.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NielsenConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for your Nielsen configuration. If you don\'t do Nielsen
-- measurement and analytics, ignore these settings. When you enable
-- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
-- to ID3 tagging for all outputs in the job. To enable Nielsen
-- configuration programmatically, include an instance of
-- nielsenConfiguration in your JSON job specification. Even if you don\'t
-- include any children of nielsenConfiguration, you still enable the
-- setting.
--
-- /See:/ 'newNielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { -- | Nielsen has discontinued the use of breakout code functionality. If you
    -- must include this property, set the value to zero.
    breakoutCode :: Prelude.Maybe Prelude.Natural,
    -- | Use Distributor ID (DistributorID) to specify the distributor ID that is
    -- assigned to your organization by Neilsen.
    distributorId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NielsenConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'breakoutCode', 'nielsenConfiguration_breakoutCode' - Nielsen has discontinued the use of breakout code functionality. If you
-- must include this property, set the value to zero.
--
-- 'distributorId', 'nielsenConfiguration_distributorId' - Use Distributor ID (DistributorID) to specify the distributor ID that is
-- assigned to your organization by Neilsen.
newNielsenConfiguration ::
  NielsenConfiguration
newNielsenConfiguration =
  NielsenConfiguration'
    { breakoutCode =
        Prelude.Nothing,
      distributorId = Prelude.Nothing
    }

-- | Nielsen has discontinued the use of breakout code functionality. If you
-- must include this property, set the value to zero.
nielsenConfiguration_breakoutCode :: Lens.Lens' NielsenConfiguration (Prelude.Maybe Prelude.Natural)
nielsenConfiguration_breakoutCode = Lens.lens (\NielsenConfiguration' {breakoutCode} -> breakoutCode) (\s@NielsenConfiguration' {} a -> s {breakoutCode = a} :: NielsenConfiguration)

-- | Use Distributor ID (DistributorID) to specify the distributor ID that is
-- assigned to your organization by Neilsen.
nielsenConfiguration_distributorId :: Lens.Lens' NielsenConfiguration (Prelude.Maybe Prelude.Text)
nielsenConfiguration_distributorId = Lens.lens (\NielsenConfiguration' {distributorId} -> distributorId) (\s@NielsenConfiguration' {} a -> s {distributorId = a} :: NielsenConfiguration)

instance Data.FromJSON NielsenConfiguration where
  parseJSON =
    Data.withObject
      "NielsenConfiguration"
      ( \x ->
          NielsenConfiguration'
            Prelude.<$> (x Data..:? "breakoutCode")
            Prelude.<*> (x Data..:? "distributorId")
      )

instance Prelude.Hashable NielsenConfiguration where
  hashWithSalt _salt NielsenConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` breakoutCode
      `Prelude.hashWithSalt` distributorId

instance Prelude.NFData NielsenConfiguration where
  rnf NielsenConfiguration' {..} =
    Prelude.rnf breakoutCode `Prelude.seq`
      Prelude.rnf distributorId

instance Data.ToJSON NielsenConfiguration where
  toJSON NielsenConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("breakoutCode" Data..=) Prelude.<$> breakoutCode,
            ("distributorId" Data..=) Prelude.<$> distributorId
          ]
      )
