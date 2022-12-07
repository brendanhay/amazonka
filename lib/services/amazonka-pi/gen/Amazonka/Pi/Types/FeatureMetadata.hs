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
-- Module      : Amazonka.Pi.Types.FeatureMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.FeatureMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pi.Types.FeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | The metadata for a feature. For example, the metadata might indicate
-- that a feature is turned on or off on a specific DB instance.
--
-- /See:/ 'newFeatureMetadata' smart constructor.
data FeatureMetadata = FeatureMetadata'
  { -- | The status of the feature on the DB instance. Possible values include
    -- the following:
    --
    -- -   @ENABLED@ - The feature is enabled on the instance.
    --
    -- -   @DISABLED@ - The feature is disabled on the instance.
    --
    -- -   @UNSUPPORTED@ - The feature isn\'t supported on the instance.
    --
    -- -   @ENABLED_PENDING_REBOOT@ - The feature is enabled on the instance
    --     but requires a reboot to take effect.
    --
    -- -   @DISABLED_PENDING_REBOOT@ - The feature is disabled on the instance
    --     but requires a reboot to take effect.
    --
    -- -   @UNKNOWN@ - The feature status couldn\'t be determined.
    status :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'featureMetadata_status' - The status of the feature on the DB instance. Possible values include
-- the following:
--
-- -   @ENABLED@ - The feature is enabled on the instance.
--
-- -   @DISABLED@ - The feature is disabled on the instance.
--
-- -   @UNSUPPORTED@ - The feature isn\'t supported on the instance.
--
-- -   @ENABLED_PENDING_REBOOT@ - The feature is enabled on the instance
--     but requires a reboot to take effect.
--
-- -   @DISABLED_PENDING_REBOOT@ - The feature is disabled on the instance
--     but requires a reboot to take effect.
--
-- -   @UNKNOWN@ - The feature status couldn\'t be determined.
newFeatureMetadata ::
  FeatureMetadata
newFeatureMetadata =
  FeatureMetadata' {status = Prelude.Nothing}

-- | The status of the feature on the DB instance. Possible values include
-- the following:
--
-- -   @ENABLED@ - The feature is enabled on the instance.
--
-- -   @DISABLED@ - The feature is disabled on the instance.
--
-- -   @UNSUPPORTED@ - The feature isn\'t supported on the instance.
--
-- -   @ENABLED_PENDING_REBOOT@ - The feature is enabled on the instance
--     but requires a reboot to take effect.
--
-- -   @DISABLED_PENDING_REBOOT@ - The feature is disabled on the instance
--     but requires a reboot to take effect.
--
-- -   @UNKNOWN@ - The feature status couldn\'t be determined.
featureMetadata_status :: Lens.Lens' FeatureMetadata (Prelude.Maybe FeatureStatus)
featureMetadata_status = Lens.lens (\FeatureMetadata' {status} -> status) (\s@FeatureMetadata' {} a -> s {status = a} :: FeatureMetadata)

instance Data.FromJSON FeatureMetadata where
  parseJSON =
    Data.withObject
      "FeatureMetadata"
      ( \x ->
          FeatureMetadata' Prelude.<$> (x Data..:? "Status")
      )

instance Prelude.Hashable FeatureMetadata where
  hashWithSalt _salt FeatureMetadata' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData FeatureMetadata where
  rnf FeatureMetadata' {..} = Prelude.rnf status
