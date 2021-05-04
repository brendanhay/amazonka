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
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a CreateObject response operation.
--
-- /See:/ 'newBatchCreateObjectResponse' smart constructor.
data BatchCreateObjectResponse = BatchCreateObjectResponse'
  { -- | The ID that is associated with the object.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ID that is associated with the object.
batchCreateObjectResponse_objectIdentifier :: Lens.Lens' BatchCreateObjectResponse (Prelude.Maybe Prelude.Text)
batchCreateObjectResponse_objectIdentifier = Lens.lens (\BatchCreateObjectResponse' {objectIdentifier} -> objectIdentifier) (\s@BatchCreateObjectResponse' {} a -> s {objectIdentifier = a} :: BatchCreateObjectResponse)

instance Prelude.FromJSON BatchCreateObjectResponse where
  parseJSON =
    Prelude.withObject
      "BatchCreateObjectResponse"
      ( \x ->
          BatchCreateObjectResponse'
            Prelude.<$> (x Prelude..:? "ObjectIdentifier")
      )

instance Prelude.Hashable BatchCreateObjectResponse

instance Prelude.NFData BatchCreateObjectResponse
