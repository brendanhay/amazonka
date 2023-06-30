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
-- Module      : Amazonka.CodeArtifact.Types.PackageOriginRestrictions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageOriginRestrictions where

import Amazonka.CodeArtifact.Types.AllowPublish
import Amazonka.CodeArtifact.Types.AllowUpstream
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the origin restrictions set on the package. The package
-- origin restrictions determine how new versions of a package can be added
-- to a specific repository.
--
-- /See:/ 'newPackageOriginRestrictions' smart constructor.
data PackageOriginRestrictions = PackageOriginRestrictions'
  { -- | The package origin configuration that determines if new versions of the
    -- package can be published directly to the repository.
    publish :: AllowPublish,
    -- | The package origin configuration that determines if new versions of the
    -- package can be added to the repository from an external connection or
    -- upstream source.
    upstream :: AllowUpstream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageOriginRestrictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publish', 'packageOriginRestrictions_publish' - The package origin configuration that determines if new versions of the
-- package can be published directly to the repository.
--
-- 'upstream', 'packageOriginRestrictions_upstream' - The package origin configuration that determines if new versions of the
-- package can be added to the repository from an external connection or
-- upstream source.
newPackageOriginRestrictions ::
  -- | 'publish'
  AllowPublish ->
  -- | 'upstream'
  AllowUpstream ->
  PackageOriginRestrictions
newPackageOriginRestrictions pPublish_ pUpstream_ =
  PackageOriginRestrictions'
    { publish = pPublish_,
      upstream = pUpstream_
    }

-- | The package origin configuration that determines if new versions of the
-- package can be published directly to the repository.
packageOriginRestrictions_publish :: Lens.Lens' PackageOriginRestrictions AllowPublish
packageOriginRestrictions_publish = Lens.lens (\PackageOriginRestrictions' {publish} -> publish) (\s@PackageOriginRestrictions' {} a -> s {publish = a} :: PackageOriginRestrictions)

-- | The package origin configuration that determines if new versions of the
-- package can be added to the repository from an external connection or
-- upstream source.
packageOriginRestrictions_upstream :: Lens.Lens' PackageOriginRestrictions AllowUpstream
packageOriginRestrictions_upstream = Lens.lens (\PackageOriginRestrictions' {upstream} -> upstream) (\s@PackageOriginRestrictions' {} a -> s {upstream = a} :: PackageOriginRestrictions)

instance Data.FromJSON PackageOriginRestrictions where
  parseJSON =
    Data.withObject
      "PackageOriginRestrictions"
      ( \x ->
          PackageOriginRestrictions'
            Prelude.<$> (x Data..: "publish")
            Prelude.<*> (x Data..: "upstream")
      )

instance Prelude.Hashable PackageOriginRestrictions where
  hashWithSalt _salt PackageOriginRestrictions' {..} =
    _salt
      `Prelude.hashWithSalt` publish
      `Prelude.hashWithSalt` upstream

instance Prelude.NFData PackageOriginRestrictions where
  rnf PackageOriginRestrictions' {..} =
    Prelude.rnf publish
      `Prelude.seq` Prelude.rnf upstream

instance Data.ToJSON PackageOriginRestrictions where
  toJSON PackageOriginRestrictions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("publish" Data..= publish),
            Prelude.Just ("upstream" Data..= upstream)
          ]
      )
