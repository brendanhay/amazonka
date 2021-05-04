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
-- Module      : Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The result of a batch add facet to object operation.
--
-- /See:/ 'newBatchAddFacetToObjectResponse' smart constructor.
data BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchAddFacetToObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchAddFacetToObjectResponse ::
  BatchAddFacetToObjectResponse
newBatchAddFacetToObjectResponse =
  BatchAddFacetToObjectResponse'

instance
  Prelude.FromJSON
    BatchAddFacetToObjectResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchAddFacetToObjectResponse"
      (\x -> Prelude.pure BatchAddFacetToObjectResponse')

instance
  Prelude.Hashable
    BatchAddFacetToObjectResponse

instance Prelude.NFData BatchAddFacetToObjectResponse
