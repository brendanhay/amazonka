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
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a DetachFromIndex response operation.
--
-- /See:/ 'newBatchDetachFromIndexResponse' smart constructor.
data BatchDetachFromIndexResponse = BatchDetachFromIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was detached from the index.
    detachedObjectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The @ObjectIdentifier@ of the object that was detached from the index.
batchDetachFromIndexResponse_detachedObjectIdentifier :: Lens.Lens' BatchDetachFromIndexResponse (Prelude.Maybe Prelude.Text)
batchDetachFromIndexResponse_detachedObjectIdentifier = Lens.lens (\BatchDetachFromIndexResponse' {detachedObjectIdentifier} -> detachedObjectIdentifier) (\s@BatchDetachFromIndexResponse' {} a -> s {detachedObjectIdentifier = a} :: BatchDetachFromIndexResponse)

instance
  Prelude.FromJSON
    BatchDetachFromIndexResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchDetachFromIndexResponse"
      ( \x ->
          BatchDetachFromIndexResponse'
            Prelude.<$> (x Prelude..:? "DetachedObjectIdentifier")
      )

instance
  Prelude.Hashable
    BatchDetachFromIndexResponse

instance Prelude.NFData BatchDetachFromIndexResponse
