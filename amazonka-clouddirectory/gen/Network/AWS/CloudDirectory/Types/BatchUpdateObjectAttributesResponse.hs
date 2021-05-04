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
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @BatchUpdate@ response operation.
--
-- /See:/ 'newBatchUpdateObjectAttributesResponse' smart constructor.
data BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse'
  { -- | ID that is associated with the object.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | ID that is associated with the object.
batchUpdateObjectAttributesResponse_objectIdentifier :: Lens.Lens' BatchUpdateObjectAttributesResponse (Prelude.Maybe Prelude.Text)
batchUpdateObjectAttributesResponse_objectIdentifier = Lens.lens (\BatchUpdateObjectAttributesResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchUpdateObjectAttributesResponse' {} a -> s {objectIdentifier = a} :: BatchUpdateObjectAttributesResponse)

instance
  Prelude.FromJSON
    BatchUpdateObjectAttributesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchUpdateObjectAttributesResponse"
      ( \x ->
          BatchUpdateObjectAttributesResponse'
            Prelude.<$> (x Prelude..:? "ObjectIdentifier")
      )

instance
  Prelude.Hashable
    BatchUpdateObjectAttributesResponse

instance
  Prelude.NFData
    BatchUpdateObjectAttributesResponse
