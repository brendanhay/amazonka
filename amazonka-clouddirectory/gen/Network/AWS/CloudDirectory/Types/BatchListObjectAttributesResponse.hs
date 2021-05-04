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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListObjectAttributes response operation.
--
-- /See:/ 'newBatchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The attributes map that is associated with the object. @AttributeArn@ is
    -- the key; attribute value is the value.
    attributes :: Prelude.Maybe [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectAttributesResponse_nextToken' - The pagination token.
--
-- 'attributes', 'batchListObjectAttributesResponse_attributes' - The attributes map that is associated with the object. @AttributeArn@ is
-- the key; attribute value is the value.
newBatchListObjectAttributesResponse ::
  BatchListObjectAttributesResponse
newBatchListObjectAttributesResponse =
  BatchListObjectAttributesResponse'
    { nextToken =
        Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | The pagination token.
batchListObjectAttributesResponse_nextToken :: Lens.Lens' BatchListObjectAttributesResponse (Prelude.Maybe Prelude.Text)
batchListObjectAttributesResponse_nextToken = Lens.lens (\BatchListObjectAttributesResponse' {nextToken} -> nextToken) (\s@BatchListObjectAttributesResponse' {} a -> s {nextToken = a} :: BatchListObjectAttributesResponse)

-- | The attributes map that is associated with the object. @AttributeArn@ is
-- the key; attribute value is the value.
batchListObjectAttributesResponse_attributes :: Lens.Lens' BatchListObjectAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
batchListObjectAttributesResponse_attributes = Lens.lens (\BatchListObjectAttributesResponse' {attributes} -> attributes) (\s@BatchListObjectAttributesResponse' {} a -> s {attributes = a} :: BatchListObjectAttributesResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchListObjectAttributesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchListObjectAttributesResponse"
      ( \x ->
          BatchListObjectAttributesResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> ( x Prelude..:? "Attributes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListObjectAttributesResponse

instance
  Prelude.NFData
    BatchListObjectAttributesResponse
