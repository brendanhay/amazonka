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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListOutgoingTypedLinks response operation.
--
-- /See:/ 'newBatchListOutgoingTypedLinksResponse' smart constructor.
data BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Prelude.Maybe [TypedLinkSpecifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchListOutgoingTypedLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListOutgoingTypedLinksResponse_nextToken' - The pagination token.
--
-- 'typedLinkSpecifiers', 'batchListOutgoingTypedLinksResponse_typedLinkSpecifiers' - Returns a typed link specifier as output.
newBatchListOutgoingTypedLinksResponse ::
  BatchListOutgoingTypedLinksResponse
newBatchListOutgoingTypedLinksResponse =
  BatchListOutgoingTypedLinksResponse'
    { nextToken =
        Prelude.Nothing,
      typedLinkSpecifiers = Prelude.Nothing
    }

-- | The pagination token.
batchListOutgoingTypedLinksResponse_nextToken :: Lens.Lens' BatchListOutgoingTypedLinksResponse (Prelude.Maybe Prelude.Text)
batchListOutgoingTypedLinksResponse_nextToken = Lens.lens (\BatchListOutgoingTypedLinksResponse' {nextToken} -> nextToken) (\s@BatchListOutgoingTypedLinksResponse' {} a -> s {nextToken = a} :: BatchListOutgoingTypedLinksResponse)

-- | Returns a typed link specifier as output.
batchListOutgoingTypedLinksResponse_typedLinkSpecifiers :: Lens.Lens' BatchListOutgoingTypedLinksResponse (Prelude.Maybe [TypedLinkSpecifier])
batchListOutgoingTypedLinksResponse_typedLinkSpecifiers = Lens.lens (\BatchListOutgoingTypedLinksResponse' {typedLinkSpecifiers} -> typedLinkSpecifiers) (\s@BatchListOutgoingTypedLinksResponse' {} a -> s {typedLinkSpecifiers = a} :: BatchListOutgoingTypedLinksResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchListOutgoingTypedLinksResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchListOutgoingTypedLinksResponse"
      ( \x ->
          BatchListOutgoingTypedLinksResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> ( x Prelude..:? "TypedLinkSpecifiers"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListOutgoingTypedLinksResponse

instance
  Prelude.NFData
    BatchListOutgoingTypedLinksResponse
