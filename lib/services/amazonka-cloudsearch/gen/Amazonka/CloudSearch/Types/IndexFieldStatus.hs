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
-- Module      : Amazonka.CloudSearch.Types.IndexFieldStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.IndexFieldStatus where

import Amazonka.CloudSearch.Types.IndexField
import Amazonka.CloudSearch.Types.OptionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The value of an @IndexField@ and its current status.
--
-- /See:/ 'newIndexFieldStatus' smart constructor.
data IndexFieldStatus = IndexFieldStatus'
  { options :: IndexField,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexFieldStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'indexFieldStatus_options' - Undocumented member.
--
-- 'status', 'indexFieldStatus_status' - Undocumented member.
newIndexFieldStatus ::
  -- | 'options'
  IndexField ->
  -- | 'status'
  OptionStatus ->
  IndexFieldStatus
newIndexFieldStatus pOptions_ pStatus_ =
  IndexFieldStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Undocumented member.
indexFieldStatus_options :: Lens.Lens' IndexFieldStatus IndexField
indexFieldStatus_options = Lens.lens (\IndexFieldStatus' {options} -> options) (\s@IndexFieldStatus' {} a -> s {options = a} :: IndexFieldStatus)

-- | Undocumented member.
indexFieldStatus_status :: Lens.Lens' IndexFieldStatus OptionStatus
indexFieldStatus_status = Lens.lens (\IndexFieldStatus' {status} -> status) (\s@IndexFieldStatus' {} a -> s {status = a} :: IndexFieldStatus)

instance Core.FromXML IndexFieldStatus where
  parseXML x =
    IndexFieldStatus'
      Prelude.<$> (x Core..@ "Options")
      Prelude.<*> (x Core..@ "Status")

instance Prelude.Hashable IndexFieldStatus where
  hashWithSalt _salt IndexFieldStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData IndexFieldStatus where
  rnf IndexFieldStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
