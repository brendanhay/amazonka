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
-- Module      : Amazonka.ConnectCases.Types.CaseEventIncludedData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.CaseEventIncludedData where

import Amazonka.ConnectCases.Types.FieldIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of what case data is published through the case event stream.
--
-- /See:/ 'newCaseEventIncludedData' smart constructor.
data CaseEventIncludedData = CaseEventIncludedData'
  { -- | List of field identifiers.
    fields :: [FieldIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaseEventIncludedData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fields', 'caseEventIncludedData_fields' - List of field identifiers.
newCaseEventIncludedData ::
  CaseEventIncludedData
newCaseEventIncludedData =
  CaseEventIncludedData' {fields = Prelude.mempty}

-- | List of field identifiers.
caseEventIncludedData_fields :: Lens.Lens' CaseEventIncludedData [FieldIdentifier]
caseEventIncludedData_fields = Lens.lens (\CaseEventIncludedData' {fields} -> fields) (\s@CaseEventIncludedData' {} a -> s {fields = a} :: CaseEventIncludedData) Prelude.. Lens.coerced

instance Data.FromJSON CaseEventIncludedData where
  parseJSON =
    Data.withObject
      "CaseEventIncludedData"
      ( \x ->
          CaseEventIncludedData'
            Prelude.<$> (x Data..:? "fields" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CaseEventIncludedData where
  hashWithSalt _salt CaseEventIncludedData' {..} =
    _salt `Prelude.hashWithSalt` fields

instance Prelude.NFData CaseEventIncludedData where
  rnf CaseEventIncludedData' {..} = Prelude.rnf fields

instance Data.ToJSON CaseEventIncludedData where
  toJSON CaseEventIncludedData' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fields" Data..= fields)]
      )
