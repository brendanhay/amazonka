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
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a DetachFromIndex response operation.
--
-- /See:/ 'newBatchDetachFromIndexResponse' smart constructor.
data BatchDetachFromIndexResponse = BatchDetachFromIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was detached from the index.
    detachedObjectIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetachFromIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detachedObjectIdentifier', 'batchDetachFromIndexResponse_detachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was detached from the index.
newBatchDetachFromIndexResponse ::
  BatchDetachFromIndexResponse
newBatchDetachFromIndexResponse =
  BatchDetachFromIndexResponse'
    { detachedObjectIdentifier =
        Core.Nothing
    }

-- | The @ObjectIdentifier@ of the object that was detached from the index.
batchDetachFromIndexResponse_detachedObjectIdentifier :: Lens.Lens' BatchDetachFromIndexResponse (Core.Maybe Core.Text)
batchDetachFromIndexResponse_detachedObjectIdentifier = Lens.lens (\BatchDetachFromIndexResponse' {detachedObjectIdentifier} -> detachedObjectIdentifier) (\s@BatchDetachFromIndexResponse' {} a -> s {detachedObjectIdentifier = a} :: BatchDetachFromIndexResponse)

instance Core.FromJSON BatchDetachFromIndexResponse where
  parseJSON =
    Core.withObject
      "BatchDetachFromIndexResponse"
      ( \x ->
          BatchDetachFromIndexResponse'
            Core.<$> (x Core..:? "DetachedObjectIdentifier")
      )

instance Core.Hashable BatchDetachFromIndexResponse

instance Core.NFData BatchDetachFromIndexResponse
