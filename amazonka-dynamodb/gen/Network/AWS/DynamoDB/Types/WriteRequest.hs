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
-- Module      : Network.AWS.DynamoDB.Types.WriteRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.WriteRequest where

import Network.AWS.DynamoDB.Types.DeleteRequest
import Network.AWS.DynamoDB.Types.PutRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an operation to perform - either @DeleteItem@ or @PutItem@.
-- You can only request one of these operations, not both, in a single
-- @WriteRequest@. If you do need to perform both of these operations, you
-- need to provide two separate @WriteRequest@ objects.
--
-- /See:/ 'newWriteRequest' smart constructor.
data WriteRequest = WriteRequest'
  { -- | A request to perform a @DeleteItem@ operation.
    deleteRequest :: Prelude.Maybe DeleteRequest,
    -- | A request to perform a @PutItem@ operation.
    putRequest :: Prelude.Maybe PutRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WriteRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteRequest', 'writeRequest_deleteRequest' - A request to perform a @DeleteItem@ operation.
--
-- 'putRequest', 'writeRequest_putRequest' - A request to perform a @PutItem@ operation.
newWriteRequest ::
  WriteRequest
newWriteRequest =
  WriteRequest'
    { deleteRequest = Prelude.Nothing,
      putRequest = Prelude.Nothing
    }

-- | A request to perform a @DeleteItem@ operation.
writeRequest_deleteRequest :: Lens.Lens' WriteRequest (Prelude.Maybe DeleteRequest)
writeRequest_deleteRequest = Lens.lens (\WriteRequest' {deleteRequest} -> deleteRequest) (\s@WriteRequest' {} a -> s {deleteRequest = a} :: WriteRequest)

-- | A request to perform a @PutItem@ operation.
writeRequest_putRequest :: Lens.Lens' WriteRequest (Prelude.Maybe PutRequest)
writeRequest_putRequest = Lens.lens (\WriteRequest' {putRequest} -> putRequest) (\s@WriteRequest' {} a -> s {putRequest = a} :: WriteRequest)

instance Prelude.FromJSON WriteRequest where
  parseJSON =
    Prelude.withObject
      "WriteRequest"
      ( \x ->
          WriteRequest'
            Prelude.<$> (x Prelude..:? "DeleteRequest")
            Prelude.<*> (x Prelude..:? "PutRequest")
      )

instance Prelude.Hashable WriteRequest

instance Prelude.NFData WriteRequest

instance Prelude.ToJSON WriteRequest where
  toJSON WriteRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeleteRequest" Prelude..=)
              Prelude.<$> deleteRequest,
            ("PutRequest" Prelude..=) Prelude.<$> putRequest
          ]
      )
