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
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output batch AttachObject response operation.
--
-- /See:/ 'newBatchAttachObjectResponse' smart constructor.
data BatchAttachObjectResponse = BatchAttachObjectResponse'
  { -- | The @ObjectIdentifier@ of the object that has been attached.
    attachedObjectIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchAttachObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedObjectIdentifier', 'batchAttachObjectResponse_attachedObjectIdentifier' - The @ObjectIdentifier@ of the object that has been attached.
newBatchAttachObjectResponse ::
  BatchAttachObjectResponse
newBatchAttachObjectResponse =
  BatchAttachObjectResponse'
    { attachedObjectIdentifier =
        Core.Nothing
    }

-- | The @ObjectIdentifier@ of the object that has been attached.
batchAttachObjectResponse_attachedObjectIdentifier :: Lens.Lens' BatchAttachObjectResponse (Core.Maybe Core.Text)
batchAttachObjectResponse_attachedObjectIdentifier = Lens.lens (\BatchAttachObjectResponse' {attachedObjectIdentifier} -> attachedObjectIdentifier) (\s@BatchAttachObjectResponse' {} a -> s {attachedObjectIdentifier = a} :: BatchAttachObjectResponse)

instance Core.FromJSON BatchAttachObjectResponse where
  parseJSON =
    Core.withObject
      "BatchAttachObjectResponse"
      ( \x ->
          BatchAttachObjectResponse'
            Core.<$> (x Core..:? "attachedObjectIdentifier")
      )

instance Core.Hashable BatchAttachObjectResponse

instance Core.NFData BatchAttachObjectResponse
