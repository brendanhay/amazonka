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
-- Module      : Network.AWS.SSM.Types.OpsItemEventSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemEventSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OpsItemIdentity

-- | Summary information about an OpsItem event.
--
-- /See:/ 'newOpsItemEventSummary' smart constructor.
data OpsItemEventSummary = OpsItemEventSummary'
  { -- | The ID of the OpsItem event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The type of information provided as a detail.
    detailType :: Prelude.Maybe Prelude.Text,
    -- | The source of the OpsItem event.
    source :: Prelude.Maybe Prelude.Text,
    -- | The date and time the OpsItem event was created.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the OpsItem.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | Specific information about the OpsItem event.
    detail :: Prelude.Maybe Prelude.Text,
    -- | Information about the user or resource that created the OpsItem event.
    createdBy :: Prelude.Maybe OpsItemIdentity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OpsItemEventSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'opsItemEventSummary_eventId' - The ID of the OpsItem event.
--
-- 'detailType', 'opsItemEventSummary_detailType' - The type of information provided as a detail.
--
-- 'source', 'opsItemEventSummary_source' - The source of the OpsItem event.
--
-- 'createdTime', 'opsItemEventSummary_createdTime' - The date and time the OpsItem event was created.
--
-- 'opsItemId', 'opsItemEventSummary_opsItemId' - The ID of the OpsItem.
--
-- 'detail', 'opsItemEventSummary_detail' - Specific information about the OpsItem event.
--
-- 'createdBy', 'opsItemEventSummary_createdBy' - Information about the user or resource that created the OpsItem event.
newOpsItemEventSummary ::
  OpsItemEventSummary
newOpsItemEventSummary =
  OpsItemEventSummary'
    { eventId = Prelude.Nothing,
      detailType = Prelude.Nothing,
      source = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      detail = Prelude.Nothing,
      createdBy = Prelude.Nothing
    }

-- | The ID of the OpsItem event.
opsItemEventSummary_eventId :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_eventId = Lens.lens (\OpsItemEventSummary' {eventId} -> eventId) (\s@OpsItemEventSummary' {} a -> s {eventId = a} :: OpsItemEventSummary)

-- | The type of information provided as a detail.
opsItemEventSummary_detailType :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_detailType = Lens.lens (\OpsItemEventSummary' {detailType} -> detailType) (\s@OpsItemEventSummary' {} a -> s {detailType = a} :: OpsItemEventSummary)

-- | The source of the OpsItem event.
opsItemEventSummary_source :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_source = Lens.lens (\OpsItemEventSummary' {source} -> source) (\s@OpsItemEventSummary' {} a -> s {source = a} :: OpsItemEventSummary)

-- | The date and time the OpsItem event was created.
opsItemEventSummary_createdTime :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.UTCTime)
opsItemEventSummary_createdTime = Lens.lens (\OpsItemEventSummary' {createdTime} -> createdTime) (\s@OpsItemEventSummary' {} a -> s {createdTime = a} :: OpsItemEventSummary) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the OpsItem.
opsItemEventSummary_opsItemId :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_opsItemId = Lens.lens (\OpsItemEventSummary' {opsItemId} -> opsItemId) (\s@OpsItemEventSummary' {} a -> s {opsItemId = a} :: OpsItemEventSummary)

-- | Specific information about the OpsItem event.
opsItemEventSummary_detail :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe Prelude.Text)
opsItemEventSummary_detail = Lens.lens (\OpsItemEventSummary' {detail} -> detail) (\s@OpsItemEventSummary' {} a -> s {detail = a} :: OpsItemEventSummary)

-- | Information about the user or resource that created the OpsItem event.
opsItemEventSummary_createdBy :: Lens.Lens' OpsItemEventSummary (Prelude.Maybe OpsItemIdentity)
opsItemEventSummary_createdBy = Lens.lens (\OpsItemEventSummary' {createdBy} -> createdBy) (\s@OpsItemEventSummary' {} a -> s {createdBy = a} :: OpsItemEventSummary)

instance Prelude.FromJSON OpsItemEventSummary where
  parseJSON =
    Prelude.withObject
      "OpsItemEventSummary"
      ( \x ->
          OpsItemEventSummary'
            Prelude.<$> (x Prelude..:? "EventId")
            Prelude.<*> (x Prelude..:? "DetailType")
            Prelude.<*> (x Prelude..:? "Source")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "OpsItemId")
            Prelude.<*> (x Prelude..:? "Detail")
            Prelude.<*> (x Prelude..:? "CreatedBy")
      )

instance Prelude.Hashable OpsItemEventSummary

instance Prelude.NFData OpsItemEventSummary
