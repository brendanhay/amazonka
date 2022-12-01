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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.GenericRevisionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an application revision.
--
-- /See:/ 'newGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { -- | The deployment groups for which this is the current target revision.
    deploymentGroups :: Prelude.Maybe [Prelude.Text],
    -- | When the revision was registered with CodeDeploy.
    registerTime :: Prelude.Maybe Core.POSIX,
    -- | A comment about the revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the revision was first used by CodeDeploy.
    firstUsedTime :: Prelude.Maybe Core.POSIX,
    -- | When the revision was last used by CodeDeploy.
    lastUsedTime :: Prelude.Maybe Core.POSIX
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
-- 'registerTime', 'genericRevisionInfo_registerTime' - When the revision was registered with CodeDeploy.
--
-- 'description', 'genericRevisionInfo_description' - A comment about the revision.
--
-- 'firstUsedTime', 'genericRevisionInfo_firstUsedTime' - When the revision was first used by CodeDeploy.
--
-- 'lastUsedTime', 'genericRevisionInfo_lastUsedTime' - When the revision was last used by CodeDeploy.
newGenericRevisionInfo ::
  GenericRevisionInfo
newGenericRevisionInfo =
  GenericRevisionInfo'
    { deploymentGroups =
        Prelude.Nothing,
      registerTime = Prelude.Nothing,
      description = Prelude.Nothing,
      firstUsedTime = Prelude.Nothing,
      lastUsedTime = Prelude.Nothing
    }

-- | The deployment groups for which this is the current target revision.
genericRevisionInfo_deploymentGroups :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe [Prelude.Text])
genericRevisionInfo_deploymentGroups = Lens.lens (\GenericRevisionInfo' {deploymentGroups} -> deploymentGroups) (\s@GenericRevisionInfo' {} a -> s {deploymentGroups = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Lens.coerced

-- | When the revision was registered with CodeDeploy.
genericRevisionInfo_registerTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_registerTime = Lens.lens (\GenericRevisionInfo' {registerTime} -> registerTime) (\s@GenericRevisionInfo' {} a -> s {registerTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Core._Time

-- | A comment about the revision.
genericRevisionInfo_description :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.Text)
genericRevisionInfo_description = Lens.lens (\GenericRevisionInfo' {description} -> description) (\s@GenericRevisionInfo' {} a -> s {description = a} :: GenericRevisionInfo)

-- | When the revision was first used by CodeDeploy.
genericRevisionInfo_firstUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_firstUsedTime = Lens.lens (\GenericRevisionInfo' {firstUsedTime} -> firstUsedTime) (\s@GenericRevisionInfo' {} a -> s {firstUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Core._Time

-- | When the revision was last used by CodeDeploy.
genericRevisionInfo_lastUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_lastUsedTime = Lens.lens (\GenericRevisionInfo' {lastUsedTime} -> lastUsedTime) (\s@GenericRevisionInfo' {} a -> s {lastUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON GenericRevisionInfo where
  parseJSON =
    Core.withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            Prelude.<$> ( x Core..:? "deploymentGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "registerTime")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "firstUsedTime")
            Prelude.<*> (x Core..:? "lastUsedTime")
      )

instance Prelude.Hashable GenericRevisionInfo where
  hashWithSalt _salt GenericRevisionInfo' {..} =
    _salt `Prelude.hashWithSalt` deploymentGroups
      `Prelude.hashWithSalt` registerTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` firstUsedTime
      `Prelude.hashWithSalt` lastUsedTime

instance Prelude.NFData GenericRevisionInfo where
  rnf GenericRevisionInfo' {..} =
    Prelude.rnf deploymentGroups
      `Prelude.seq` Prelude.rnf registerTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf firstUsedTime
      `Prelude.seq` Prelude.rnf lastUsedTime
