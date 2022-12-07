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
-- Module      : Amazonka.AppStream.Types.EntitledApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.EntitledApplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The application associated to an entitlement. Access is controlled based
-- on user attributes.
--
-- /See:/ 'newEntitledApplication' smart constructor.
data EntitledApplication = EntitledApplication'
  { -- | The identifier of the application.
    applicationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitledApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIdentifier', 'entitledApplication_applicationIdentifier' - The identifier of the application.
newEntitledApplication ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  EntitledApplication
newEntitledApplication pApplicationIdentifier_ =
  EntitledApplication'
    { applicationIdentifier =
        pApplicationIdentifier_
    }

-- | The identifier of the application.
entitledApplication_applicationIdentifier :: Lens.Lens' EntitledApplication Prelude.Text
entitledApplication_applicationIdentifier = Lens.lens (\EntitledApplication' {applicationIdentifier} -> applicationIdentifier) (\s@EntitledApplication' {} a -> s {applicationIdentifier = a} :: EntitledApplication)

instance Data.FromJSON EntitledApplication where
  parseJSON =
    Data.withObject
      "EntitledApplication"
      ( \x ->
          EntitledApplication'
            Prelude.<$> (x Data..: "ApplicationIdentifier")
      )

instance Prelude.Hashable EntitledApplication where
  hashWithSalt _salt EntitledApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationIdentifier

instance Prelude.NFData EntitledApplication where
  rnf EntitledApplication' {..} =
    Prelude.rnf applicationIdentifier
