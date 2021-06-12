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
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a @BatchUpdate@ response operation.
--
-- /See:/ 'newBatchUpdateObjectAttributesResponse' smart constructor.
data BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse'
  { -- | ID that is associated with the object.
    objectIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchUpdateObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'batchUpdateObjectAttributesResponse_objectIdentifier' - ID that is associated with the object.
newBatchUpdateObjectAttributesResponse ::
  BatchUpdateObjectAttributesResponse
newBatchUpdateObjectAttributesResponse =
  BatchUpdateObjectAttributesResponse'
    { objectIdentifier =
        Core.Nothing
    }

-- | ID that is associated with the object.
batchUpdateObjectAttributesResponse_objectIdentifier :: Lens.Lens' BatchUpdateObjectAttributesResponse (Core.Maybe Core.Text)
batchUpdateObjectAttributesResponse_objectIdentifier = Lens.lens (\BatchUpdateObjectAttributesResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchUpdateObjectAttributesResponse' {} a -> s {objectIdentifier = a} :: BatchUpdateObjectAttributesResponse)

instance
  Core.FromJSON
    BatchUpdateObjectAttributesResponse
  where
  parseJSON =
    Core.withObject
      "BatchUpdateObjectAttributesResponse"
      ( \x ->
          BatchUpdateObjectAttributesResponse'
            Core.<$> (x Core..:? "ObjectIdentifier")
      )

instance
  Core.Hashable
    BatchUpdateObjectAttributesResponse

instance
  Core.NFData
    BatchUpdateObjectAttributesResponse
