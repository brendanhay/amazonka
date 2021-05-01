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
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a CreateIndex response operation.
--
-- /See:/ 'newBatchCreateIndexResponse' smart constructor.
data BatchCreateIndexResponse = BatchCreateIndexResponse'
  { -- | The @ObjectIdentifier@ of the index created by this operation.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'batchCreateIndexResponse_objectIdentifier' - The @ObjectIdentifier@ of the index created by this operation.
newBatchCreateIndexResponse ::
  BatchCreateIndexResponse
newBatchCreateIndexResponse =
  BatchCreateIndexResponse'
    { objectIdentifier =
        Prelude.Nothing
    }

-- | The @ObjectIdentifier@ of the index created by this operation.
batchCreateIndexResponse_objectIdentifier :: Lens.Lens' BatchCreateIndexResponse (Prelude.Maybe Prelude.Text)
batchCreateIndexResponse_objectIdentifier = Lens.lens (\BatchCreateIndexResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchCreateIndexResponse' {} a -> s {objectIdentifier = a} :: BatchCreateIndexResponse)

instance Prelude.FromJSON BatchCreateIndexResponse where
  parseJSON =
    Prelude.withObject
      "BatchCreateIndexResponse"
      ( \x ->
          BatchCreateIndexResponse'
            Prelude.<$> (x Prelude..:? "ObjectIdentifier")
      )

instance Prelude.Hashable BatchCreateIndexResponse

instance Prelude.NFData BatchCreateIndexResponse
