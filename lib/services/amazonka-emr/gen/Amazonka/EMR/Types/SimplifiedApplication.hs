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
-- Module      : Amazonka.EMR.Types.SimplifiedApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SimplifiedApplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The returned release label application names or versions.
--
-- /See:/ 'newSimplifiedApplication' smart constructor.
data SimplifiedApplication = SimplifiedApplication'
  { -- | The returned release label application name. For example, @hadoop@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The returned release label application version. For example, @3.2.1@.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimplifiedApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'simplifiedApplication_name' - The returned release label application name. For example, @hadoop@.
--
-- 'version', 'simplifiedApplication_version' - The returned release label application version. For example, @3.2.1@.
newSimplifiedApplication ::
  SimplifiedApplication
newSimplifiedApplication =
  SimplifiedApplication'
    { name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The returned release label application name. For example, @hadoop@.
simplifiedApplication_name :: Lens.Lens' SimplifiedApplication (Prelude.Maybe Prelude.Text)
simplifiedApplication_name = Lens.lens (\SimplifiedApplication' {name} -> name) (\s@SimplifiedApplication' {} a -> s {name = a} :: SimplifiedApplication)

-- | The returned release label application version. For example, @3.2.1@.
simplifiedApplication_version :: Lens.Lens' SimplifiedApplication (Prelude.Maybe Prelude.Text)
simplifiedApplication_version = Lens.lens (\SimplifiedApplication' {version} -> version) (\s@SimplifiedApplication' {} a -> s {version = a} :: SimplifiedApplication)

instance Data.FromJSON SimplifiedApplication where
  parseJSON =
    Data.withObject
      "SimplifiedApplication"
      ( \x ->
          SimplifiedApplication'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable SimplifiedApplication where
  hashWithSalt _salt SimplifiedApplication' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData SimplifiedApplication where
  rnf SimplifiedApplication' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version
