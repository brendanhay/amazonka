{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an application revision.
--
-- /See:/ 'newGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { -- | When the revision was registered with AWS CodeDeploy.
    registerTime :: Prelude.Maybe Prelude.POSIX,
    -- | The deployment groups for which this is the current target revision.
    deploymentGroups :: Prelude.Maybe [Prelude.Text],
    -- | A comment about the revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the revision was first used by AWS CodeDeploy.
    firstUsedTime :: Prelude.Maybe Prelude.POSIX,
    -- | When the revision was last used by AWS CodeDeploy.
    lastUsedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'deploymentGroups', 'genericRevisionInfo_deploymentGroups' - The deployment groups for which this is the current target revision.
--
-- 'description', 'genericRevisionInfo_description' - A comment about the revision.
--
-- 'firstUsedTime', 'genericRevisionInfo_firstUsedTime' - When the revision was first used by AWS CodeDeploy.
--
-- 'lastUsedTime', 'genericRevisionInfo_lastUsedTime' - When the revision was last used by AWS CodeDeploy.
newGenericRevisionInfo ::
  GenericRevisionInfo
newGenericRevisionInfo =
  GenericRevisionInfo'
    { registerTime =
        Prelude.Nothing,
      deploymentGroups = Prelude.Nothing,
      description = Prelude.Nothing,
      firstUsedTime = Prelude.Nothing,
      lastUsedTime = Prelude.Nothing
    }

-- | When the revision was registered with AWS CodeDeploy.
genericRevisionInfo_registerTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_registerTime = Lens.lens (\GenericRevisionInfo' {registerTime} -> registerTime) (\s@GenericRevisionInfo' {} a -> s {registerTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Prelude._Time

-- | The deployment groups for which this is the current target revision.
genericRevisionInfo_deploymentGroups :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe [Prelude.Text])
genericRevisionInfo_deploymentGroups = Lens.lens (\GenericRevisionInfo' {deploymentGroups} -> deploymentGroups) (\s@GenericRevisionInfo' {} a -> s {deploymentGroups = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | A comment about the revision.
genericRevisionInfo_description :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.Text)
genericRevisionInfo_description = Lens.lens (\GenericRevisionInfo' {description} -> description) (\s@GenericRevisionInfo' {} a -> s {description = a} :: GenericRevisionInfo)

-- | When the revision was first used by AWS CodeDeploy.
genericRevisionInfo_firstUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_firstUsedTime = Lens.lens (\GenericRevisionInfo' {firstUsedTime} -> firstUsedTime) (\s@GenericRevisionInfo' {} a -> s {firstUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Prelude._Time

-- | When the revision was last used by AWS CodeDeploy.
genericRevisionInfo_lastUsedTime :: Lens.Lens' GenericRevisionInfo (Prelude.Maybe Prelude.UTCTime)
genericRevisionInfo_lastUsedTime = Lens.lens (\GenericRevisionInfo' {lastUsedTime} -> lastUsedTime) (\s@GenericRevisionInfo' {} a -> s {lastUsedTime = a} :: GenericRevisionInfo) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON GenericRevisionInfo where
  parseJSON =
    Prelude.withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            Prelude.<$> (x Prelude..:? "registerTime")
            Prelude.<*> ( x Prelude..:? "deploymentGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "firstUsedTime")
            Prelude.<*> (x Prelude..:? "lastUsedTime")
      )

instance Prelude.Hashable GenericRevisionInfo

instance Prelude.NFData GenericRevisionInfo
