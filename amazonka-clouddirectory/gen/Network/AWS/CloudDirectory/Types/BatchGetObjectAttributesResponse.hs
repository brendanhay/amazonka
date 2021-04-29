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
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a GetObjectAttributes response operation.
--
-- /See:/ 'newBatchGetObjectAttributesResponse' smart constructor.
data BatchGetObjectAttributesResponse = BatchGetObjectAttributesResponse'
  { -- | The attribute values that are associated with an object.
    attributes :: Prelude.Maybe [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'batchGetObjectAttributesResponse_attributes' - The attribute values that are associated with an object.
newBatchGetObjectAttributesResponse ::
  BatchGetObjectAttributesResponse
newBatchGetObjectAttributesResponse =
  BatchGetObjectAttributesResponse'
    { attributes =
        Prelude.Nothing
    }

-- | The attribute values that are associated with an object.
batchGetObjectAttributesResponse_attributes :: Lens.Lens' BatchGetObjectAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
batchGetObjectAttributesResponse_attributes = Lens.lens (\BatchGetObjectAttributesResponse' {attributes} -> attributes) (\s@BatchGetObjectAttributesResponse' {} a -> s {attributes = a} :: BatchGetObjectAttributesResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchGetObjectAttributesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchGetObjectAttributesResponse"
      ( \x ->
          BatchGetObjectAttributesResponse'
            Prelude.<$> ( x Prelude..:? "Attributes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchGetObjectAttributesResponse

instance
  Prelude.NFData
    BatchGetObjectAttributesResponse
