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
-- Module      : Amazonka.MacieV2.Types.ClassificationScopeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationScopeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the classification scope for an Amazon Macie
-- account. Macie uses the scope\'s settings when it performs automated
-- sensitive data discovery for the account.
--
-- /See:/ 'newClassificationScopeSummary' smart constructor.
data ClassificationScopeSummary = ClassificationScopeSummary'
  { -- | The unique identifier for the classification scope.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the classification scope.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationScopeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'classificationScopeSummary_id' - The unique identifier for the classification scope.
--
-- 'name', 'classificationScopeSummary_name' - The name of the classification scope.
newClassificationScopeSummary ::
  ClassificationScopeSummary
newClassificationScopeSummary =
  ClassificationScopeSummary'
    { id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The unique identifier for the classification scope.
classificationScopeSummary_id :: Lens.Lens' ClassificationScopeSummary (Prelude.Maybe Prelude.Text)
classificationScopeSummary_id = Lens.lens (\ClassificationScopeSummary' {id} -> id) (\s@ClassificationScopeSummary' {} a -> s {id = a} :: ClassificationScopeSummary)

-- | The name of the classification scope.
classificationScopeSummary_name :: Lens.Lens' ClassificationScopeSummary (Prelude.Maybe Prelude.Text)
classificationScopeSummary_name = Lens.lens (\ClassificationScopeSummary' {name} -> name) (\s@ClassificationScopeSummary' {} a -> s {name = a} :: ClassificationScopeSummary)

instance Data.FromJSON ClassificationScopeSummary where
  parseJSON =
    Data.withObject
      "ClassificationScopeSummary"
      ( \x ->
          ClassificationScopeSummary'
            Prelude.<$> (x Data..:? "id") Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ClassificationScopeSummary where
  hashWithSalt _salt ClassificationScopeSummary' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ClassificationScopeSummary where
  rnf ClassificationScopeSummary' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
