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
-- Module      : Amazonka.CloudDirectory.Types.BatchCreateObjectResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchCreateObjectResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a CreateObject response operation.
--
-- /See:/ 'newBatchCreateObjectResponse' smart constructor.
data BatchCreateObjectResponse = BatchCreateObjectResponse'
  { -- | The ID that is associated with the object.
    objectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON BatchCreateObjectResponse where
  parseJSON =
    Data.withObject
      "BatchCreateObjectResponse"
      ( \x ->
          BatchCreateObjectResponse'
            Prelude.<$> (x Data..:? "ObjectIdentifier")
      )

instance Prelude.Hashable BatchCreateObjectResponse where
  hashWithSalt _salt BatchCreateObjectResponse' {..} =
    _salt `Prelude.hashWithSalt` objectIdentifier

instance Prelude.NFData BatchCreateObjectResponse where
  rnf BatchCreateObjectResponse' {..} =
    Prelude.rnf objectIdentifier
