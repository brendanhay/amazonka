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
-- Module      : Amazonka.IotTwinMaker.Types.SyncResourceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncResourceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.SyncResourceState
import Amazonka.IotTwinMaker.Types.SyncResourceType
import qualified Amazonka.Prelude as Prelude

-- | The sync resource filter.
--
-- /See:/ 'newSyncResourceFilter' smart constructor.
data SyncResourceFilter = SyncResourceFilter'
  { -- | The external Id.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The sync resource filter resource Id.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The sync resource filter resoucre type
    resourceType :: Prelude.Maybe SyncResourceType,
    -- | The sync resource filter\'s state.
    state :: Prelude.Maybe SyncResourceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncResourceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'syncResourceFilter_externalId' - The external Id.
--
-- 'resourceId', 'syncResourceFilter_resourceId' - The sync resource filter resource Id.
--
-- 'resourceType', 'syncResourceFilter_resourceType' - The sync resource filter resoucre type
--
-- 'state', 'syncResourceFilter_state' - The sync resource filter\'s state.
newSyncResourceFilter ::
  SyncResourceFilter
newSyncResourceFilter =
  SyncResourceFilter'
    { externalId = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The external Id.
syncResourceFilter_externalId :: Lens.Lens' SyncResourceFilter (Prelude.Maybe Prelude.Text)
syncResourceFilter_externalId = Lens.lens (\SyncResourceFilter' {externalId} -> externalId) (\s@SyncResourceFilter' {} a -> s {externalId = a} :: SyncResourceFilter)

-- | The sync resource filter resource Id.
syncResourceFilter_resourceId :: Lens.Lens' SyncResourceFilter (Prelude.Maybe Prelude.Text)
syncResourceFilter_resourceId = Lens.lens (\SyncResourceFilter' {resourceId} -> resourceId) (\s@SyncResourceFilter' {} a -> s {resourceId = a} :: SyncResourceFilter)

-- | The sync resource filter resoucre type
syncResourceFilter_resourceType :: Lens.Lens' SyncResourceFilter (Prelude.Maybe SyncResourceType)
syncResourceFilter_resourceType = Lens.lens (\SyncResourceFilter' {resourceType} -> resourceType) (\s@SyncResourceFilter' {} a -> s {resourceType = a} :: SyncResourceFilter)

-- | The sync resource filter\'s state.
syncResourceFilter_state :: Lens.Lens' SyncResourceFilter (Prelude.Maybe SyncResourceState)
syncResourceFilter_state = Lens.lens (\SyncResourceFilter' {state} -> state) (\s@SyncResourceFilter' {} a -> s {state = a} :: SyncResourceFilter)

instance Prelude.Hashable SyncResourceFilter where
  hashWithSalt _salt SyncResourceFilter' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` state

instance Prelude.NFData SyncResourceFilter where
  rnf SyncResourceFilter' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf state

instance Data.ToJSON SyncResourceFilter where
  toJSON SyncResourceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("externalId" Data..=) Prelude.<$> externalId,
            ("resourceId" Data..=) Prelude.<$> resourceId,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            ("state" Data..=) Prelude.<$> state
          ]
      )
