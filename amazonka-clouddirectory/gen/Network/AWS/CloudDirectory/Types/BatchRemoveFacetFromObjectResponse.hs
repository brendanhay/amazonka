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
-- Module      : Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An empty result that represents success.
--
-- /See:/ 'newBatchRemoveFacetFromObjectResponse' smart constructor.
data BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchRemoveFacetFromObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchRemoveFacetFromObjectResponse ::
  BatchRemoveFacetFromObjectResponse
newBatchRemoveFacetFromObjectResponse =
  BatchRemoveFacetFromObjectResponse'

instance
  Core.FromJSON
    BatchRemoveFacetFromObjectResponse
  where
  parseJSON =
    Core.withObject
      "BatchRemoveFacetFromObjectResponse"
      ( \x ->
          Core.pure BatchRemoveFacetFromObjectResponse'
      )

instance
  Core.Hashable
    BatchRemoveFacetFromObjectResponse

instance
  Core.NFData
    BatchRemoveFacetFromObjectResponse
