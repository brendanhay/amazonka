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
-- Module      : Network.AWS.Translate.Types.TerminologyDataLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyDataLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The location of the custom terminology data.
--
-- /See:/ 'newTerminologyDataLocation' smart constructor.
data TerminologyDataLocation = TerminologyDataLocation'
  { -- | The repository type for the custom terminology data.
    repositoryType :: Core.Text,
    -- | The location of the custom terminology data.
    location :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminologyDataLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryType', 'terminologyDataLocation_repositoryType' - The repository type for the custom terminology data.
--
-- 'location', 'terminologyDataLocation_location' - The location of the custom terminology data.
newTerminologyDataLocation ::
  -- | 'repositoryType'
  Core.Text ->
  -- | 'location'
  Core.Text ->
  TerminologyDataLocation
newTerminologyDataLocation
  pRepositoryType_
  pLocation_ =
    TerminologyDataLocation'
      { repositoryType =
          pRepositoryType_,
        location = pLocation_
      }

-- | The repository type for the custom terminology data.
terminologyDataLocation_repositoryType :: Lens.Lens' TerminologyDataLocation Core.Text
terminologyDataLocation_repositoryType = Lens.lens (\TerminologyDataLocation' {repositoryType} -> repositoryType) (\s@TerminologyDataLocation' {} a -> s {repositoryType = a} :: TerminologyDataLocation)

-- | The location of the custom terminology data.
terminologyDataLocation_location :: Lens.Lens' TerminologyDataLocation Core.Text
terminologyDataLocation_location = Lens.lens (\TerminologyDataLocation' {location} -> location) (\s@TerminologyDataLocation' {} a -> s {location = a} :: TerminologyDataLocation)

instance Core.FromJSON TerminologyDataLocation where
  parseJSON =
    Core.withObject
      "TerminologyDataLocation"
      ( \x ->
          TerminologyDataLocation'
            Core.<$> (x Core..: "RepositoryType")
            Core.<*> (x Core..: "Location")
      )

instance Core.Hashable TerminologyDataLocation

instance Core.NFData TerminologyDataLocation
