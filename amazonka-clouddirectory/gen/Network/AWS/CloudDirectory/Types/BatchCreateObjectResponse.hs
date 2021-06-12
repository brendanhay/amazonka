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
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a CreateObject response operation.
--
-- /See:/ 'newBatchCreateObjectResponse' smart constructor.
data BatchCreateObjectResponse = BatchCreateObjectResponse'
  { -- | The ID that is associated with the object.
    objectIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchCreateObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'batchCreateObjectResponse_objectIdentifier' - The ID that is associated with the object.
newBatchCreateObjectResponse ::
  BatchCreateObjectResponse
newBatchCreateObjectResponse =
  BatchCreateObjectResponse'
    { objectIdentifier =
        Core.Nothing
    }

-- | The ID that is associated with the object.
batchCreateObjectResponse_objectIdentifier :: Lens.Lens' BatchCreateObjectResponse (Core.Maybe Core.Text)
batchCreateObjectResponse_objectIdentifier = Lens.lens (\BatchCreateObjectResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchCreateObjectResponse' {} a -> s {objectIdentifier = a} :: BatchCreateObjectResponse)

instance Core.FromJSON BatchCreateObjectResponse where
  parseJSON =
    Core.withObject
      "BatchCreateObjectResponse"
      ( \x ->
          BatchCreateObjectResponse'
            Core.<$> (x Core..:? "ObjectIdentifier")
      )

instance Core.Hashable BatchCreateObjectResponse

instance Core.NFData BatchCreateObjectResponse
