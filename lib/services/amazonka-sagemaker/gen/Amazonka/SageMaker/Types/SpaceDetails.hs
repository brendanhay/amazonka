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
-- Module      : Amazonka.SageMaker.Types.SpaceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SpaceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.SpaceStatus

-- | The space\'s details.
--
-- /See:/ 'newSpaceDetails' smart constructor.
data SpaceDetails = SpaceDetails'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the associated Domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the space.
    spaceName :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe SpaceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpaceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'spaceDetails_creationTime' - The creation time.
--
-- 'domainId', 'spaceDetails_domainId' - The ID of the associated Domain.
--
-- 'lastModifiedTime', 'spaceDetails_lastModifiedTime' - The last modified time.
--
-- 'spaceName', 'spaceDetails_spaceName' - The name of the space.
--
-- 'status', 'spaceDetails_status' - The status.
newSpaceDetails ::
  SpaceDetails
newSpaceDetails =
  SpaceDetails'
    { creationTime = Prelude.Nothing,
      domainId = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      spaceName = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The creation time.
spaceDetails_creationTime :: Lens.Lens' SpaceDetails (Prelude.Maybe Prelude.UTCTime)
spaceDetails_creationTime = Lens.lens (\SpaceDetails' {creationTime} -> creationTime) (\s@SpaceDetails' {} a -> s {creationTime = a} :: SpaceDetails) Prelude.. Lens.mapping Data._Time

-- | The ID of the associated Domain.
spaceDetails_domainId :: Lens.Lens' SpaceDetails (Prelude.Maybe Prelude.Text)
spaceDetails_domainId = Lens.lens (\SpaceDetails' {domainId} -> domainId) (\s@SpaceDetails' {} a -> s {domainId = a} :: SpaceDetails)

-- | The last modified time.
spaceDetails_lastModifiedTime :: Lens.Lens' SpaceDetails (Prelude.Maybe Prelude.UTCTime)
spaceDetails_lastModifiedTime = Lens.lens (\SpaceDetails' {lastModifiedTime} -> lastModifiedTime) (\s@SpaceDetails' {} a -> s {lastModifiedTime = a} :: SpaceDetails) Prelude.. Lens.mapping Data._Time

-- | The name of the space.
spaceDetails_spaceName :: Lens.Lens' SpaceDetails (Prelude.Maybe Prelude.Text)
spaceDetails_spaceName = Lens.lens (\SpaceDetails' {spaceName} -> spaceName) (\s@SpaceDetails' {} a -> s {spaceName = a} :: SpaceDetails)

-- | The status.
spaceDetails_status :: Lens.Lens' SpaceDetails (Prelude.Maybe SpaceStatus)
spaceDetails_status = Lens.lens (\SpaceDetails' {status} -> status) (\s@SpaceDetails' {} a -> s {status = a} :: SpaceDetails)

instance Data.FromJSON SpaceDetails where
  parseJSON =
    Data.withObject
      "SpaceDetails"
      ( \x ->
          SpaceDetails'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "SpaceName")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable SpaceDetails where
  hashWithSalt _salt SpaceDetails' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` spaceName
      `Prelude.hashWithSalt` status

instance Prelude.NFData SpaceDetails where
  rnf SpaceDetails' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf spaceName
      `Prelude.seq` Prelude.rnf status
