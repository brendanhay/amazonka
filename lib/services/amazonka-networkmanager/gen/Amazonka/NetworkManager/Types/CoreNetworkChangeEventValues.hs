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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkChangeEventValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkChangeEventValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network change event.
--
-- /See:/ 'newCoreNetworkChangeEventValues' smart constructor.
data CoreNetworkChangeEventValues = CoreNetworkChangeEventValues'
  { -- | For a @STATIC_ROUTE@ event, this is the IP address.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The edge location for the core network change event.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment if the change event is associated with an
    -- attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The segment name if the change event is associated with a segment.
    segmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkChangeEventValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'coreNetworkChangeEventValues_cidr' - For a @STATIC_ROUTE@ event, this is the IP address.
--
-- 'edgeLocation', 'coreNetworkChangeEventValues_edgeLocation' - The edge location for the core network change event.
--
-- 'attachmentId', 'coreNetworkChangeEventValues_attachmentId' - The ID of the attachment if the change event is associated with an
-- attachment.
--
-- 'segmentName', 'coreNetworkChangeEventValues_segmentName' - The segment name if the change event is associated with a segment.
newCoreNetworkChangeEventValues ::
  CoreNetworkChangeEventValues
newCoreNetworkChangeEventValues =
  CoreNetworkChangeEventValues'
    { cidr =
        Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      segmentName = Prelude.Nothing
    }

-- | For a @STATIC_ROUTE@ event, this is the IP address.
coreNetworkChangeEventValues_cidr :: Lens.Lens' CoreNetworkChangeEventValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeEventValues_cidr = Lens.lens (\CoreNetworkChangeEventValues' {cidr} -> cidr) (\s@CoreNetworkChangeEventValues' {} a -> s {cidr = a} :: CoreNetworkChangeEventValues)

-- | The edge location for the core network change event.
coreNetworkChangeEventValues_edgeLocation :: Lens.Lens' CoreNetworkChangeEventValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeEventValues_edgeLocation = Lens.lens (\CoreNetworkChangeEventValues' {edgeLocation} -> edgeLocation) (\s@CoreNetworkChangeEventValues' {} a -> s {edgeLocation = a} :: CoreNetworkChangeEventValues)

-- | The ID of the attachment if the change event is associated with an
-- attachment.
coreNetworkChangeEventValues_attachmentId :: Lens.Lens' CoreNetworkChangeEventValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeEventValues_attachmentId = Lens.lens (\CoreNetworkChangeEventValues' {attachmentId} -> attachmentId) (\s@CoreNetworkChangeEventValues' {} a -> s {attachmentId = a} :: CoreNetworkChangeEventValues)

-- | The segment name if the change event is associated with a segment.
coreNetworkChangeEventValues_segmentName :: Lens.Lens' CoreNetworkChangeEventValues (Prelude.Maybe Prelude.Text)
coreNetworkChangeEventValues_segmentName = Lens.lens (\CoreNetworkChangeEventValues' {segmentName} -> segmentName) (\s@CoreNetworkChangeEventValues' {} a -> s {segmentName = a} :: CoreNetworkChangeEventValues)

instance Data.FromJSON CoreNetworkChangeEventValues where
  parseJSON =
    Data.withObject
      "CoreNetworkChangeEventValues"
      ( \x ->
          CoreNetworkChangeEventValues'
            Prelude.<$> (x Data..:? "Cidr")
            Prelude.<*> (x Data..:? "EdgeLocation")
            Prelude.<*> (x Data..:? "AttachmentId")
            Prelude.<*> (x Data..:? "SegmentName")
      )

instance
  Prelude.Hashable
    CoreNetworkChangeEventValues
  where
  hashWithSalt _salt CoreNetworkChangeEventValues' {..} =
    _salt `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` segmentName

instance Prelude.NFData CoreNetworkChangeEventValues where
  rnf CoreNetworkChangeEventValues' {..} =
    Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf segmentName
