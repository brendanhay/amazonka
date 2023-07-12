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
-- Module      : Amazonka.AuditManager.Types.Scope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Scope where

import Amazonka.AuditManager.Types.AWSAccount
import Amazonka.AuditManager.Types.AWSService
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The wrapper that contains the Amazon Web Services accounts and services
-- that are in scope for the assessment.
--
-- /See:/ 'newScope' smart constructor.
data Scope = Scope'
  { -- | The Amazon Web Services accounts that are included in the scope of the
    -- assessment.
    awsAccounts :: Prelude.Maybe [AWSAccount],
    -- | The Amazon Web Services services that are included in the scope of the
    -- assessment.
    awsServices :: Prelude.Maybe [AWSService]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccounts', 'scope_awsAccounts' - The Amazon Web Services accounts that are included in the scope of the
-- assessment.
--
-- 'awsServices', 'scope_awsServices' - The Amazon Web Services services that are included in the scope of the
-- assessment.
newScope ::
  Scope
newScope =
  Scope'
    { awsAccounts = Prelude.Nothing,
      awsServices = Prelude.Nothing
    }

-- | The Amazon Web Services accounts that are included in the scope of the
-- assessment.
scope_awsAccounts :: Lens.Lens' Scope (Prelude.Maybe [AWSAccount])
scope_awsAccounts = Lens.lens (\Scope' {awsAccounts} -> awsAccounts) (\s@Scope' {} a -> s {awsAccounts = a} :: Scope) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services services that are included in the scope of the
-- assessment.
scope_awsServices :: Lens.Lens' Scope (Prelude.Maybe [AWSService])
scope_awsServices = Lens.lens (\Scope' {awsServices} -> awsServices) (\s@Scope' {} a -> s {awsServices = a} :: Scope) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Scope where
  parseJSON =
    Data.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Data..:? "awsAccounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "awsServices" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Scope where
  hashWithSalt _salt Scope' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccounts
      `Prelude.hashWithSalt` awsServices

instance Prelude.NFData Scope where
  rnf Scope' {..} =
    Prelude.rnf awsAccounts
      `Prelude.seq` Prelude.rnf awsServices

instance Data.ToJSON Scope where
  toJSON Scope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsAccounts" Data..=) Prelude.<$> awsAccounts,
            ("awsServices" Data..=) Prelude.<$> awsServices
          ]
      )
