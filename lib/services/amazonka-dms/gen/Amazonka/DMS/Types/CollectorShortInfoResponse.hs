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
-- Module      : Amazonka.DMS.Types.CollectorShortInfoResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.CollectorShortInfoResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Briefly describes a Fleet Advisor collector.
--
-- /See:/ 'newCollectorShortInfoResponse' smart constructor.
data CollectorShortInfoResponse = CollectorShortInfoResponse'
  { -- | The name of the Fleet Advisor collector.
    collectorName :: Prelude.Maybe Prelude.Text,
    -- | The reference ID of the Fleet Advisor collector.
    collectorReferencedId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectorShortInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectorName', 'collectorShortInfoResponse_collectorName' - The name of the Fleet Advisor collector.
--
-- 'collectorReferencedId', 'collectorShortInfoResponse_collectorReferencedId' - The reference ID of the Fleet Advisor collector.
newCollectorShortInfoResponse ::
  CollectorShortInfoResponse
newCollectorShortInfoResponse =
  CollectorShortInfoResponse'
    { collectorName =
        Prelude.Nothing,
      collectorReferencedId = Prelude.Nothing
    }

-- | The name of the Fleet Advisor collector.
collectorShortInfoResponse_collectorName :: Lens.Lens' CollectorShortInfoResponse (Prelude.Maybe Prelude.Text)
collectorShortInfoResponse_collectorName = Lens.lens (\CollectorShortInfoResponse' {collectorName} -> collectorName) (\s@CollectorShortInfoResponse' {} a -> s {collectorName = a} :: CollectorShortInfoResponse)

-- | The reference ID of the Fleet Advisor collector.
collectorShortInfoResponse_collectorReferencedId :: Lens.Lens' CollectorShortInfoResponse (Prelude.Maybe Prelude.Text)
collectorShortInfoResponse_collectorReferencedId = Lens.lens (\CollectorShortInfoResponse' {collectorReferencedId} -> collectorReferencedId) (\s@CollectorShortInfoResponse' {} a -> s {collectorReferencedId = a} :: CollectorShortInfoResponse)

instance Core.FromJSON CollectorShortInfoResponse where
  parseJSON =
    Core.withObject
      "CollectorShortInfoResponse"
      ( \x ->
          CollectorShortInfoResponse'
            Prelude.<$> (x Core..:? "CollectorName")
            Prelude.<*> (x Core..:? "CollectorReferencedId")
      )

instance Prelude.Hashable CollectorShortInfoResponse where
  hashWithSalt _salt CollectorShortInfoResponse' {..} =
    _salt `Prelude.hashWithSalt` collectorName
      `Prelude.hashWithSalt` collectorReferencedId

instance Prelude.NFData CollectorShortInfoResponse where
  rnf CollectorShortInfoResponse' {..} =
    Prelude.rnf collectorName
      `Prelude.seq` Prelude.rnf collectorReferencedId
