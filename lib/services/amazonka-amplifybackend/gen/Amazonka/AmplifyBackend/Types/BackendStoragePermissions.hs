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
-- Module      : Amazonka.AmplifyBackend.Types.BackendStoragePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendStoragePermissions where

import Amazonka.AmplifyBackend.Types.AuthenticatedElement
import Amazonka.AmplifyBackend.Types.UnAuthenticatedElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the read, write, and delete permissions users have against
-- your storage S3 bucket.
--
-- /See:/ 'newBackendStoragePermissions' smart constructor.
data BackendStoragePermissions = BackendStoragePermissions'
  { -- | Lists all unauthenticated user read, write, and delete permissions for
    -- your S3 bucket.
    unAuthenticated :: Prelude.Maybe [UnAuthenticatedElement],
    -- | Lists all authenticated user read, write, and delete permissions for
    -- your S3 bucket.
    authenticated :: [AuthenticatedElement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendStoragePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unAuthenticated', 'backendStoragePermissions_unAuthenticated' - Lists all unauthenticated user read, write, and delete permissions for
-- your S3 bucket.
--
-- 'authenticated', 'backendStoragePermissions_authenticated' - Lists all authenticated user read, write, and delete permissions for
-- your S3 bucket.
newBackendStoragePermissions ::
  BackendStoragePermissions
newBackendStoragePermissions =
  BackendStoragePermissions'
    { unAuthenticated =
        Prelude.Nothing,
      authenticated = Prelude.mempty
    }

-- | Lists all unauthenticated user read, write, and delete permissions for
-- your S3 bucket.
backendStoragePermissions_unAuthenticated :: Lens.Lens' BackendStoragePermissions (Prelude.Maybe [UnAuthenticatedElement])
backendStoragePermissions_unAuthenticated = Lens.lens (\BackendStoragePermissions' {unAuthenticated} -> unAuthenticated) (\s@BackendStoragePermissions' {} a -> s {unAuthenticated = a} :: BackendStoragePermissions) Prelude.. Lens.mapping Lens.coerced

-- | Lists all authenticated user read, write, and delete permissions for
-- your S3 bucket.
backendStoragePermissions_authenticated :: Lens.Lens' BackendStoragePermissions [AuthenticatedElement]
backendStoragePermissions_authenticated = Lens.lens (\BackendStoragePermissions' {authenticated} -> authenticated) (\s@BackendStoragePermissions' {} a -> s {authenticated = a} :: BackendStoragePermissions) Prelude.. Lens.coerced

instance Data.FromJSON BackendStoragePermissions where
  parseJSON =
    Data.withObject
      "BackendStoragePermissions"
      ( \x ->
          BackendStoragePermissions'
            Prelude.<$> ( x
                            Data..:? "unAuthenticated"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "authenticated" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BackendStoragePermissions where
  hashWithSalt _salt BackendStoragePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` unAuthenticated
      `Prelude.hashWithSalt` authenticated

instance Prelude.NFData BackendStoragePermissions where
  rnf BackendStoragePermissions' {..} =
    Prelude.rnf unAuthenticated
      `Prelude.seq` Prelude.rnf authenticated

instance Data.ToJSON BackendStoragePermissions where
  toJSON BackendStoragePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("unAuthenticated" Data..=)
              Prelude.<$> unAuthenticated,
            Prelude.Just
              ("authenticated" Data..= authenticated)
          ]
      )
