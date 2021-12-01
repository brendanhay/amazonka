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
-- Module      : Amazonka.SSM.Types.OpsItemEventSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemEventSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemIdentity

-- | Summary information about an OpsItem event or that associated an OpsItem
-- with a related item.
--
-- /See:/ 'newOpsItemEventSummary' smart constructor.
data OpsItemEventSummary = OpsItemEventSummary'
  { -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the OpsItem event was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the user or resource that created the OpsItem event.
    createdBy :: Prelude.Maybe OpsItemIdentity,
    -- | The type of information provided as a detail.
    detailType :: Prelude.Maybe Prelude.Text,
    -- | The source of the OpsItem event.
    source :: Prelude.Maybe Prelude.Text,
    -- | Specific information about the OpsItem event.
    detail :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OpsItem event.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemEventSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemId', 'opsItemEventSummary_opsItemId' - The ID of the OpsItem.
--
-- 'createdTime', 'opsItemEventSummary_createdTime' - The date and time the OpsItem event was created.
--
-- 'createdBy', 'opsItemEventSummary_createdBy' - Information about the user or resource that created the OpsItem event.
--
-- 'detailType', 'opsItemEventSummary_detailType' - The type of information provided as a detail.
--
-- 'source', 'opsItemEventSummary_source' - The source of the OpsItem event.
--
-- 'detail', 'opsItemEventSummary_detail' - Specific information about the OpsItem event.
--
-- 'eventId', 'opsItemEventSummary_eventId' - The ID of the OpsItem event.
newOpsItemEventSummary ::
  OpsItemEventSummary
newOpsItemEventSummary =
  OpsItemEventSummary'
    { opsItemId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      detailType = Prelude.Nothing,
      source = Prelude.Nothing,
      detail = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | The ID of the OpsItem.
opsItemEventSummary_opsItemId :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_opsItemId = Lens.lens (\OpsItemEventSummary' {opsItemId} -> opsItemId) (\s@OpsItemEventSummary' {} a -> s {opsItemId = a} :: OpsItemEventSummary)

-- | The date and time the OpsItem event was created.
opsItemEventSummary_createdTime :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.UTCTime)
opsItemEventSummary_createdTime = Lens.lens (\OpsItemEventSummary' {createdTime} -> createdTime) (\s@OpsItemEventSummary' {} a -> s {createdTime = a} :: OpsItemEventSummary) Prelude.. Lens.mapping Core._Time

-- | Information about the user or resource that created the OpsItem event.
opsItemEventSummary_createdBy :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe OpsItemIdentity)
opsItemEventSummary_createdBy = Lens.lens (\OpsItemEventSummary' {createdBy} -> createdBy) (\s@OpsItemEventSummary' {} a -> s {createdBy = a} :: OpsItemEventSummary)

-- | The type of information provided as a detail.
opsItemEventSummary_detailType :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_detailType = Lens.lens (\OpsItemEventSummary' {detailType} -> detailType) (\s@OpsItemEventSummary' {} a -> s {detailType = a} :: OpsItemEventSummary)

-- | The source of the OpsItem event.
opsItemEventSummary_source :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_source = Lens.lens (\OpsItemEventSummary' {source} -> source) (\s@OpsItemEventSummary' {} a -> s {source = a} :: OpsItemEventSummary)

-- | Specific information about the OpsItem event.
opsItemEventSummary_detail :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_detail = Lens.lens (\OpsItemEventSummary' {detail} -> detail) (\s@OpsItemEventSummary' {} a -> s {detail = a} :: OpsItemEventSummary)

-- | The ID of the OpsItem event.
opsItemEventSummary_eventId :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_eventId = Lens.lens (\OpsItemEventSummary' {eventId} -> eventId) (\s@OpsItemEventSummary' {} a -> s {eventId = a} :: OpsItemEventSummary)

instance Core.FromJSON OpsItemEventSummary where
  parseJSON =
    Core.withObject
      "OpsItemEventSummary"
      ( \x ->
          OpsItemEventSummary'
            Prelude.<$> (x Core..:? "OpsItemId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "DetailType")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "Detail")
            Prelude.<*> (x Core..:? "EventId")
      )

instance Prelude.Hashable OpsItemEventSummary where
  hashWithSalt salt' OpsItemEventSummary' {..} =
    salt' `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` detail
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` detailType
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` opsItemId

instance Prelude.NFData OpsItemEventSummary where
  rnf OpsItemEventSummary' {..} =
    Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf detail
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf detailType
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdTime
