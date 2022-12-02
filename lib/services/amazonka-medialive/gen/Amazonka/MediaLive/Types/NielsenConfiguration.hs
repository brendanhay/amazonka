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
-- Module      : Amazonka.MediaLive.Types.NielsenConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.NielsenPcmToId3TaggingState
import qualified Amazonka.Prelude as Prelude

-- | Nielsen Configuration
--
-- /See:/ 'newNielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { -- | Enter the Distributor ID assigned to your organization by Nielsen.
    distributorId :: Prelude.Maybe Prelude.Text,
    -- | Enables Nielsen PCM to ID3 tagging
    nielsenPcmToId3Tagging :: Prelude.Maybe NielsenPcmToId3TaggingState
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
-- 'distributorId', 'nielsenConfiguration_distributorId' - Enter the Distributor ID assigned to your organization by Nielsen.
--
-- 'nielsenPcmToId3Tagging', 'nielsenConfiguration_nielsenPcmToId3Tagging' - Enables Nielsen PCM to ID3 tagging
newNielsenConfiguration ::
  NielsenConfiguration
newNielsenConfiguration =
  NielsenConfiguration'
    { distributorId =
        Prelude.Nothing,
      nielsenPcmToId3Tagging = Prelude.Nothing
    }

-- | Enter the Distributor ID assigned to your organization by Nielsen.
nielsenConfiguration_distributorId :: Lens.Lens' NielsenConfiguration (Prelude.Maybe Prelude.Text)
nielsenConfiguration_distributorId = Lens.lens (\NielsenConfiguration' {distributorId} -> distributorId) (\s@NielsenConfiguration' {} a -> s {distributorId = a} :: NielsenConfiguration)

-- | Enables Nielsen PCM to ID3 tagging
nielsenConfiguration_nielsenPcmToId3Tagging :: Lens.Lens' NielsenConfiguration (Prelude.Maybe NielsenPcmToId3TaggingState)
nielsenConfiguration_nielsenPcmToId3Tagging = Lens.lens (\NielsenConfiguration' {nielsenPcmToId3Tagging} -> nielsenPcmToId3Tagging) (\s@NielsenConfiguration' {} a -> s {nielsenPcmToId3Tagging = a} :: NielsenConfiguration)

instance Data.FromJSON NielsenConfiguration where
  parseJSON =
    Data.withObject
      "NielsenConfiguration"
      ( \x ->
          NielsenConfiguration'
            Prelude.<$> (x Data..:? "distributorId")
            Prelude.<*> (x Data..:? "nielsenPcmToId3Tagging")
      )

instance Prelude.Hashable NielsenConfiguration where
  hashWithSalt _salt NielsenConfiguration' {..} =
    _salt `Prelude.hashWithSalt` distributorId
      `Prelude.hashWithSalt` nielsenPcmToId3Tagging

instance Prelude.NFData NielsenConfiguration where
  rnf NielsenConfiguration' {..} =
    Prelude.rnf distributorId
      `Prelude.seq` Prelude.rnf nielsenPcmToId3Tagging

instance Data.ToJSON NielsenConfiguration where
  toJSON NielsenConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("distributorId" Data..=) Prelude.<$> distributorId,
            ("nielsenPcmToId3Tagging" Data..=)
              Prelude.<$> nielsenPcmToId3Tagging
          ]
      )
