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
-- Module      : Amazonka.Textract.Types.LendingSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LendingSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.DocumentGroup

-- | Contains information regarding DocumentGroups and
-- UndetectedDocumentTypes.
--
-- /See:/ 'newLendingSummary' smart constructor.
data LendingSummary = LendingSummary'
  { -- | Contains an array of all DocumentGroup objects.
    documentGroups :: Prelude.Maybe [DocumentGroup],
    -- | UndetectedDocumentTypes.
    undetectedDocumentTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LendingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentGroups', 'lendingSummary_documentGroups' - Contains an array of all DocumentGroup objects.
--
-- 'undetectedDocumentTypes', 'lendingSummary_undetectedDocumentTypes' - UndetectedDocumentTypes.
newLendingSummary ::
  LendingSummary
newLendingSummary =
  LendingSummary'
    { documentGroups = Prelude.Nothing,
      undetectedDocumentTypes = Prelude.Nothing
    }

-- | Contains an array of all DocumentGroup objects.
lendingSummary_documentGroups :: Lens.Lens' LendingSummary (Prelude.Maybe [DocumentGroup])
lendingSummary_documentGroups = Lens.lens (\LendingSummary' {documentGroups} -> documentGroups) (\s@LendingSummary' {} a -> s {documentGroups = a} :: LendingSummary) Prelude.. Lens.mapping Lens.coerced

-- | UndetectedDocumentTypes.
lendingSummary_undetectedDocumentTypes :: Lens.Lens' LendingSummary (Prelude.Maybe [Prelude.Text])
lendingSummary_undetectedDocumentTypes = Lens.lens (\LendingSummary' {undetectedDocumentTypes} -> undetectedDocumentTypes) (\s@LendingSummary' {} a -> s {undetectedDocumentTypes = a} :: LendingSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LendingSummary where
  parseJSON =
    Data.withObject
      "LendingSummary"
      ( \x ->
          LendingSummary'
            Prelude.<$> (x Data..:? "DocumentGroups" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "UndetectedDocumentTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LendingSummary where
  hashWithSalt _salt LendingSummary' {..} =
    _salt
      `Prelude.hashWithSalt` documentGroups
      `Prelude.hashWithSalt` undetectedDocumentTypes

instance Prelude.NFData LendingSummary where
  rnf LendingSummary' {..} =
    Prelude.rnf documentGroups `Prelude.seq`
      Prelude.rnf undetectedDocumentTypes
