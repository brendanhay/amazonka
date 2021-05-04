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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a ListObjectPolicies response operation.
--
-- /See:/ 'newBatchListObjectPoliciesResponse' smart constructor.
data BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of policy @ObjectIdentifiers@, that are attached to the object.
    attachedPolicyIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectPoliciesResponse_nextToken' - The pagination token.
--
-- 'attachedPolicyIds', 'batchListObjectPoliciesResponse_attachedPolicyIds' - A list of policy @ObjectIdentifiers@, that are attached to the object.
newBatchListObjectPoliciesResponse ::
  BatchListObjectPoliciesResponse
newBatchListObjectPoliciesResponse =
  BatchListObjectPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      attachedPolicyIds = Prelude.Nothing
    }

-- | The pagination token.
batchListObjectPoliciesResponse_nextToken :: Lens.Lens' BatchListObjectPoliciesResponse (Prelude.Maybe Prelude.Text)
batchListObjectPoliciesResponse_nextToken = Lens.lens (\BatchListObjectPoliciesResponse' {nextToken} -> nextToken) (\s@BatchListObjectPoliciesResponse' {} a -> s {nextToken = a} :: BatchListObjectPoliciesResponse)

-- | A list of policy @ObjectIdentifiers@, that are attached to the object.
batchListObjectPoliciesResponse_attachedPolicyIds :: Lens.Lens' BatchListObjectPoliciesResponse (Prelude.Maybe [Prelude.Text])
batchListObjectPoliciesResponse_attachedPolicyIds = Lens.lens (\BatchListObjectPoliciesResponse' {attachedPolicyIds} -> attachedPolicyIds) (\s@BatchListObjectPoliciesResponse' {} a -> s {attachedPolicyIds = a} :: BatchListObjectPoliciesResponse) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchListObjectPoliciesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchListObjectPoliciesResponse"
      ( \x ->
          BatchListObjectPoliciesResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> ( x Prelude..:? "AttachedPolicyIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchListObjectPoliciesResponse

instance
  Prelude.NFData
    BatchListObjectPoliciesResponse
