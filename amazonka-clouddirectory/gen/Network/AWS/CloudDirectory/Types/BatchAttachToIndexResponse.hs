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
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a AttachToIndex response operation.
--
-- /See:/ 'newBatchAttachToIndexResponse' smart constructor.
data BatchAttachToIndexResponse = BatchAttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchAttachToIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedObjectIdentifier', 'batchAttachToIndexResponse_attachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
newBatchAttachToIndexResponse ::
  BatchAttachToIndexResponse
newBatchAttachToIndexResponse =
  BatchAttachToIndexResponse'
    { attachedObjectIdentifier =
        Prelude.Nothing
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
batchAttachToIndexResponse_attachedObjectIdentifier :: Lens.Lens' BatchAttachToIndexResponse (Prelude.Maybe Prelude.Text)
batchAttachToIndexResponse_attachedObjectIdentifier = Lens.lens (\BatchAttachToIndexResponse' {attachedObjectIdentifier} -> attachedObjectIdentifier) (\s@BatchAttachToIndexResponse' {} a -> s {attachedObjectIdentifier = a} :: BatchAttachToIndexResponse)

instance Prelude.FromJSON BatchAttachToIndexResponse where
  parseJSON =
    Prelude.withObject
      "BatchAttachToIndexResponse"
      ( \x ->
          BatchAttachToIndexResponse'
            Prelude.<$> (x Prelude..:? "AttachedObjectIdentifier")
      )

instance Prelude.Hashable BatchAttachToIndexResponse

instance Prelude.NFData BatchAttachToIndexResponse
