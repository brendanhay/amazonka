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
-- Module      : Network.AWS.CodeDeploy.Types.GenericRevisionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GenericRevisionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an application revision.
--
-- /See:/ 'newGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { -- | When the revision was registered with AWS CodeDeploy.
    registerTime :: Prelude.Maybe Core.POSIX,
    -- | When the revision was first used by AWS CodeDeploy.
    firstUsedTime :: Prelude.Maybe Core.POSIX,
    -- | The deployment groups for which this is the current target revision.
    deploymentGroups :: Prelude.Maybe [Prelude.Text],
    -- | When the revision was last used by AWS CodeDeploy.
    lastUsedTime :: Prelude.Maybe Core.POSIX,
    -- | A comment about the revision.
    description :: Prelude.Maybe Prelude.Text
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
-- 'registerTime', 'genericRevisionInfo_registerTime' - When the revision was registered with AWS CodeDeploy.
--
-- 'firstUsedTime', 'genericRevisionInfo_firstUsedTime' - When the revision was first used by AWS CodeDeploy.
--
-- 'deploymentGroups', 'genericRevisionInfo_deploymentGroups' - The deployment groups for which this is the current target revision.
--
-- 'lastUsedTime', 'genericRevisionInfo_lastUsedTime' - When the revision was last used by AWS CodeDeploy.
--
-- 'description', 'genericRevisionInfo_description' - A comment about the revision.
newGenericRevisionInfo ::
  GenericRevisionInfo
newGenericRevisionInfo =
  GenericRevisionInfo'
    { registerTime =
        Prelude.Nothing,
      firstUsedTime = Prelude.Nothing,
      deploymentGroups = Prelude.Nothing,
      lastUsedTime = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | When the revision was registered with AWS CodeDeploy.
genericRevisionInfo_registerTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_registerTime = Lens.lens (\GenericRevisionInfo' {registerTime} -> registerTime) (\s@GenericRevisionInfo' {} a -> s {registerTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Core._Time

-- | When the revision was first used by AWS CodeDeploy.
genericRevisionInfo_firstUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_firstUsedTime = Lens.lens (\GenericRevisionInfo' {firstUsedTime} -> firstUsedTime) (\s@GenericRevisionInfo' {} a -> s {firstUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Core._Time

-- | The deployment groups for which this is the current target revision.
genericRevisionInfo_deploymentGroups :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe [Prelude.Text])
genericRevisionInfo_deploymentGroups = Lens.lens (\GenericRevisionInfo' {deploymentGroups} -> deploymentGroups) (\s@GenericRevisionInfo' {} a -> s {deploymentGroups = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Lens.coerced

-- | When the revision was last used by AWS CodeDeploy.
genericRevisionInfo_lastUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_lastUsedTime = Lens.lens (\GenericRevisionInfo' {lastUsedTime} -> lastUsedTime) (\s@GenericRevisionInfo' {} a -> s {lastUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Core._Time

-- | A comment about the revision.
genericRevisionInfo_description :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.Text)
genericRevisionInfo_description = Lens.lens (\GenericRevisionInfo' {description} -> description) (\s@GenericRevisionInfo' {} a -> s {description = a} :: GenericRevisionInfo)

instance Core.FromJSON GenericRevisionInfo where
  parseJSON =
    Core.withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            Prelude.<$> (x Core..:? "registerTime")
            Prelude.<*> (x Core..:? "firstUsedTime")
            Prelude.<*> ( x Core..:? "deploymentGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastUsedTime")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable GenericRevisionInfo

instance Prelude.NFData GenericRevisionInfo
