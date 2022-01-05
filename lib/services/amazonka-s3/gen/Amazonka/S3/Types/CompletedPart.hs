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
-- Module      : Amazonka.S3.Types.CompletedPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CompletedPart where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Details of the parts that were uploaded.
--
-- /See:/ 'newCompletedPart' smart constructor.
data CompletedPart = CompletedPart'
  { -- | Part number that identifies the part. This is a positive integer between
    -- 1 and 10,000.
    partNumber :: Prelude.Int,
    -- | Entity tag returned when the part was uploaded.
    eTag :: ETag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompletedPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partNumber', 'completedPart_partNumber' - Part number that identifies the part. This is a positive integer between
-- 1 and 10,000.
--
-- 'eTag', 'completedPart_eTag' - Entity tag returned when the part was uploaded.
newCompletedPart ::
  -- | 'partNumber'
  Prelude.Int ->
  -- | 'eTag'
  ETag ->
  CompletedPart
newCompletedPart pPartNumber_ pETag_ =
  CompletedPart'
    { partNumber = pPartNumber_,
      eTag = pETag_
    }

-- | Part number that identifies the part. This is a positive integer between
-- 1 and 10,000.
completedPart_partNumber :: Lens.Lens' CompletedPart Prelude.Int
completedPart_partNumber = Lens.lens (\CompletedPart' {partNumber} -> partNumber) (\s@CompletedPart' {} a -> s {partNumber = a} :: CompletedPart)

-- | Entity tag returned when the part was uploaded.
completedPart_eTag :: Lens.Lens' CompletedPart ETag
completedPart_eTag = Lens.lens (\CompletedPart' {eTag} -> eTag) (\s@CompletedPart' {} a -> s {eTag = a} :: CompletedPart)

instance Prelude.Hashable CompletedPart where
  hashWithSalt _salt CompletedPart' {..} =
    _salt `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` eTag

instance Prelude.NFData CompletedPart where
  rnf CompletedPart' {..} =
    Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf eTag

instance Core.ToXML CompletedPart where
  toXML CompletedPart' {..} =
    Prelude.mconcat
      [ "PartNumber" Core.@= partNumber,
        "ETag" Core.@= eTag
      ]
