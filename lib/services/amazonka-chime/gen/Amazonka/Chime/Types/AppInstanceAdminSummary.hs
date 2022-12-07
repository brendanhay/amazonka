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
-- Module      : Amazonka.Chime.Types.AppInstanceAdminSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AppInstanceAdminSummary where

import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of an @AppInstanceAdmin@.
--
-- /See:/ 'newAppInstanceAdminSummary' smart constructor.
data AppInstanceAdminSummary = AppInstanceAdminSummary'
  { -- | The details of the @AppInstanceAdmin@.
    admin :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceAdminSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'admin', 'appInstanceAdminSummary_admin' - The details of the @AppInstanceAdmin@.
newAppInstanceAdminSummary ::
  AppInstanceAdminSummary
newAppInstanceAdminSummary =
  AppInstanceAdminSummary' {admin = Prelude.Nothing}

-- | The details of the @AppInstanceAdmin@.
appInstanceAdminSummary_admin :: Lens.Lens' AppInstanceAdminSummary (Prelude.Maybe Identity)
appInstanceAdminSummary_admin = Lens.lens (\AppInstanceAdminSummary' {admin} -> admin) (\s@AppInstanceAdminSummary' {} a -> s {admin = a} :: AppInstanceAdminSummary)

instance Data.FromJSON AppInstanceAdminSummary where
  parseJSON =
    Data.withObject
      "AppInstanceAdminSummary"
      ( \x ->
          AppInstanceAdminSummary'
            Prelude.<$> (x Data..:? "Admin")
      )

instance Prelude.Hashable AppInstanceAdminSummary where
  hashWithSalt _salt AppInstanceAdminSummary' {..} =
    _salt `Prelude.hashWithSalt` admin

instance Prelude.NFData AppInstanceAdminSummary where
  rnf AppInstanceAdminSummary' {..} = Prelude.rnf admin
