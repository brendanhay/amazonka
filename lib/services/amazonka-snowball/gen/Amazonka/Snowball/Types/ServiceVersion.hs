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
-- Module      : Amazonka.Snowball.Types.ServiceVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ServiceVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The version of the requested service.
--
-- /See:/ 'newServiceVersion' smart constructor.
data ServiceVersion = ServiceVersion'
  { -- | The version number of the requested service.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'serviceVersion_version' - The version number of the requested service.
newServiceVersion ::
  ServiceVersion
newServiceVersion =
  ServiceVersion' {version = Prelude.Nothing}

-- | The version number of the requested service.
serviceVersion_version :: Lens.Lens' ServiceVersion (Prelude.Maybe Prelude.Text)
serviceVersion_version = Lens.lens (\ServiceVersion' {version} -> version) (\s@ServiceVersion' {} a -> s {version = a} :: ServiceVersion)

instance Data.FromJSON ServiceVersion where
  parseJSON =
    Data.withObject
      "ServiceVersion"
      ( \x ->
          ServiceVersion' Prelude.<$> (x Data..:? "Version")
      )

instance Prelude.Hashable ServiceVersion where
  hashWithSalt _salt ServiceVersion' {..} =
    _salt `Prelude.hashWithSalt` version

instance Prelude.NFData ServiceVersion where
  rnf ServiceVersion' {..} = Prelude.rnf version

instance Data.ToJSON ServiceVersion where
  toJSON ServiceVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Version" Data..=) Prelude.<$> version]
      )
