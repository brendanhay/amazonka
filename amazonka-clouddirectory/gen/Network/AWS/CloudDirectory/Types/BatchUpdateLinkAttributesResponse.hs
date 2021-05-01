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
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a UpdateLinkAttributes response operation.
--
-- /See:/ 'newBatchUpdateLinkAttributesResponse' smart constructor.
data BatchUpdateLinkAttributesResponse = BatchUpdateLinkAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateLinkAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchUpdateLinkAttributesResponse ::
  BatchUpdateLinkAttributesResponse
newBatchUpdateLinkAttributesResponse =
  BatchUpdateLinkAttributesResponse'

instance
  Prelude.FromJSON
    BatchUpdateLinkAttributesResponse
  where
  parseJSON =
    Prelude.withObject
      "BatchUpdateLinkAttributesResponse"
      ( \x ->
          Prelude.pure BatchUpdateLinkAttributesResponse'
      )

instance
  Prelude.Hashable
    BatchUpdateLinkAttributesResponse

instance
  Prelude.NFData
    BatchUpdateLinkAttributesResponse
