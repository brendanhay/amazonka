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
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a DetachObject response operation.
--
-- /See:/ 'newBatchDetachObjectResponse' smart constructor.
data BatchDetachObjectResponse = BatchDetachObjectResponse'
  { -- | The @ObjectIdentifier@ of the detached object.
    detachedObjectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetachObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detachedObjectIdentifier', 'batchDetachObjectResponse_detachedObjectIdentifier' - The @ObjectIdentifier@ of the detached object.
newBatchDetachObjectResponse ::
  BatchDetachObjectResponse
newBatchDetachObjectResponse =
  BatchDetachObjectResponse'
    { detachedObjectIdentifier =
        Prelude.Nothing
    }

-- | The @ObjectIdentifier@ of the detached object.
batchDetachObjectResponse_detachedObjectIdentifier :: Lens.Lens' BatchDetachObjectResponse (Prelude.Maybe Prelude.Text)
batchDetachObjectResponse_detachedObjectIdentifier = Lens.lens (\BatchDetachObjectResponse' {detachedObjectIdentifier} -> detachedObjectIdentifier) (\s@BatchDetachObjectResponse' {} a -> s {detachedObjectIdentifier = a} :: BatchDetachObjectResponse)

instance Prelude.FromJSON BatchDetachObjectResponse where
  parseJSON =
    Prelude.withObject
      "BatchDetachObjectResponse"
      ( \x ->
          BatchDetachObjectResponse'
            Prelude.<$> (x Prelude..:? "detachedObjectIdentifier")
      )

instance Prelude.Hashable BatchDetachObjectResponse

instance Prelude.NFData BatchDetachObjectResponse
