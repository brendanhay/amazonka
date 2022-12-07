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
-- Module      : Amazonka.ConnectCases.Types.RelatedItemEventIncludedData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.RelatedItemEventIncludedData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of what related item data is published through the case event
-- stream.
--
-- /See:/ 'newRelatedItemEventIncludedData' smart constructor.
data RelatedItemEventIncludedData = RelatedItemEventIncludedData'
  { -- | Details of what related item data is published through the case event
    -- stream.
    includeContent :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedItemEventIncludedData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeContent', 'relatedItemEventIncludedData_includeContent' - Details of what related item data is published through the case event
-- stream.
newRelatedItemEventIncludedData ::
  -- | 'includeContent'
  Prelude.Bool ->
  RelatedItemEventIncludedData
newRelatedItemEventIncludedData pIncludeContent_ =
  RelatedItemEventIncludedData'
    { includeContent =
        pIncludeContent_
    }

-- | Details of what related item data is published through the case event
-- stream.
relatedItemEventIncludedData_includeContent :: Lens.Lens' RelatedItemEventIncludedData Prelude.Bool
relatedItemEventIncludedData_includeContent = Lens.lens (\RelatedItemEventIncludedData' {includeContent} -> includeContent) (\s@RelatedItemEventIncludedData' {} a -> s {includeContent = a} :: RelatedItemEventIncludedData)

instance Data.FromJSON RelatedItemEventIncludedData where
  parseJSON =
    Data.withObject
      "RelatedItemEventIncludedData"
      ( \x ->
          RelatedItemEventIncludedData'
            Prelude.<$> (x Data..: "includeContent")
      )

instance
  Prelude.Hashable
    RelatedItemEventIncludedData
  where
  hashWithSalt _salt RelatedItemEventIncludedData' {..} =
    _salt `Prelude.hashWithSalt` includeContent

instance Prelude.NFData RelatedItemEventIncludedData where
  rnf RelatedItemEventIncludedData' {..} =
    Prelude.rnf includeContent

instance Data.ToJSON RelatedItemEventIncludedData where
  toJSON RelatedItemEventIncludedData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("includeContent" Data..= includeContent)
          ]
      )
