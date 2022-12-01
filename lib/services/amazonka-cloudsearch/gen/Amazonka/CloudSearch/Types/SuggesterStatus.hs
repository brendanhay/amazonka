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
-- Module      : Amazonka.CloudSearch.Types.SuggesterStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.SuggesterStatus where

import Amazonka.CloudSearch.Types.OptionStatus
import Amazonka.CloudSearch.Types.Suggester
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The value of a @Suggester@ and its current status.
--
-- /See:/ 'newSuggesterStatus' smart constructor.
data SuggesterStatus = SuggesterStatus'
  { options :: Suggester,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggesterStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'suggesterStatus_options' - Undocumented member.
--
-- 'status', 'suggesterStatus_status' - Undocumented member.
newSuggesterStatus ::
  -- | 'options'
  Suggester ->
  -- | 'status'
  OptionStatus ->
  SuggesterStatus
newSuggesterStatus pOptions_ pStatus_ =
  SuggesterStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Undocumented member.
suggesterStatus_options :: Lens.Lens' SuggesterStatus Suggester
suggesterStatus_options = Lens.lens (\SuggesterStatus' {options} -> options) (\s@SuggesterStatus' {} a -> s {options = a} :: SuggesterStatus)

-- | Undocumented member.
suggesterStatus_status :: Lens.Lens' SuggesterStatus OptionStatus
suggesterStatus_status = Lens.lens (\SuggesterStatus' {status} -> status) (\s@SuggesterStatus' {} a -> s {status = a} :: SuggesterStatus)

instance Core.FromXML SuggesterStatus where
  parseXML x =
    SuggesterStatus'
      Prelude.<$> (x Core..@ "Options")
      Prelude.<*> (x Core..@ "Status")

instance Prelude.Hashable SuggesterStatus where
  hashWithSalt _salt SuggesterStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData SuggesterStatus where
  rnf SuggesterStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
