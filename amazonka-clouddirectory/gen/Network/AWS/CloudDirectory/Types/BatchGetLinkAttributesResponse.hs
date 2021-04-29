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
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a GetLinkAttributes response operation.
--
-- /See:/ 'newBatchGetLinkAttributesResponse' smart constructor.
data BatchGetLinkAttributesResponse = BatchGetLinkAttributesResponse'
  { -- | The attributes that are associated with the typed link.
    attributes :: Prelude.Maybe [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetLinkAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'batchGetLinkAttributesResponse_attributes' - The attributes that are associated with the typed link.
newBatchGetLinkAttributesResponse ::
  BatchGetLinkAttributesResponse
newBatchGetLinkAttributesResponse =
  BatchGetLinkAttributesResponse'
    { attributes =
        Prelude.Nothing
    }

-- | The attributes that are associated with the typed link.
batchGetLinkAttributesResponse_attributes :: Lens.Lens' BatchGetLinkAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
batchGetLinkAttributesResponse_attributes = Lens.lens (\BatchGetLinkAttributesResponse' {attributes} -> attributes) (\s@BatchGetLinkAttributesResponse' {} a -> s {attributes = a} :: BatchGetLinkAttributesResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchGetLinkAttributesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchGetLinkAttributesResponse"
      ( \x ->
          BatchGetLinkAttributesResponse'
            Prelude.<$> ( x Prelude..:? "Attributes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchGetLinkAttributesResponse

instance
  Prelude.NFData
    BatchGetLinkAttributesResponse
