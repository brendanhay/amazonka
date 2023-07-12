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
-- Module      : Amazonka.CodeDeploy.Types.GenericRevisionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.GenericRevisionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an application revision.
--
-- /See:/ 'newGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { -- | The deployment groups for which this is the current target revision.
    deploymentGroups :: Prelude.Maybe [Prelude.Text],
    -- | A comment about the revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the revision was first used by CodeDeploy.
    firstUsedTime :: Prelude.Maybe Data.POSIX,
    -- | When the revision was last used by CodeDeploy.
    lastUsedTime :: Prelude.Maybe Data.POSIX,
    -- | When the revision was registered with CodeDeploy.
    registerTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenericRevisionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentGroups', 'genericRevisionInfo_deploymentGroups' - The deployment groups for which this is the current target revision.
--
-- 'description', 'genericRevisionInfo_description' - A comment about the revision.
--
-- 'firstUsedTime', 'genericRevisionInfo_firstUsedTime' - When the revision was first used by CodeDeploy.
--
-- 'lastUsedTime', 'genericRevisionInfo_lastUsedTime' - When the revision was last used by CodeDeploy.
--
-- 'registerTime', 'genericRevisionInfo_registerTime' - When the revision was registered with CodeDeploy.
newGenericRevisionInfo ::
  GenericRevisionInfo
newGenericRevisionInfo =
  GenericRevisionInfo'
    { deploymentGroups =
        Prelude.Nothing,
      description = Prelude.Nothing,
      firstUsedTime = Prelude.Nothing,
      lastUsedTime = Prelude.Nothing,
      registerTime = Prelude.Nothing
    }

-- | The deployment groups for which this is the current target revision.
genericRevisionInfo_deploymentGroups :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe [Prelude.Text])
genericRevisionInfo_deploymentGroups = Lens.lens (\GenericRevisionInfo' {deploymentGroups} -> deploymentGroups) (\s@GenericRevisionInfo' {} a -> s {deploymentGroups = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Lens.coerced

-- | A comment about the revision.
genericRevisionInfo_description :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.Text)
genericRevisionInfo_description = Lens.lens (\GenericRevisionInfo' {description} -> description) (\s@GenericRevisionInfo' {} a -> s {description = a} :: GenericRevisionInfo)

-- | When the revision was first used by CodeDeploy.
genericRevisionInfo_firstUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_firstUsedTime = Lens.lens (\GenericRevisionInfo' {firstUsedTime} -> firstUsedTime) (\s@GenericRevisionInfo' {} a -> s {firstUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Data._Time

-- | When the revision was last used by CodeDeploy.
genericRevisionInfo_lastUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_lastUsedTime = Lens.lens (\GenericRevisionInfo' {lastUsedTime} -> lastUsedTime) (\s@GenericRevisionInfo' {} a -> s {lastUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Data._Time

-- | When the revision was registered with CodeDeploy.
genericRevisionInfo_registerTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_registerTime = Lens.lens (\GenericRevisionInfo' {registerTime} -> registerTime) (\s@GenericRevisionInfo' {} a -> s {registerTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON GenericRevisionInfo where
  parseJSON =
    Data.withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            Prelude.<$> ( x
                            Data..:? "deploymentGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "firstUsedTime")
            Prelude.<*> (x Data..:? "lastUsedTime")
            Prelude.<*> (x Data..:? "registerTime")
      )

instance Prelude.Hashable GenericRevisionInfo where
  hashWithSalt _salt GenericRevisionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentGroups
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` firstUsedTime
      `Prelude.hashWithSalt` lastUsedTime
      `Prelude.hashWithSalt` registerTime

instance Prelude.NFData GenericRevisionInfo where
  rnf GenericRevisionInfo' {..} =
    Prelude.rnf deploymentGroups
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf firstUsedTime
      `Prelude.seq` Prelude.rnf lastUsedTime
      `Prelude.seq` Prelude.rnf registerTime
