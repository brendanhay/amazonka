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
-- Module      : Network.AWS.CodeDeploy.Types.RevisionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionInfo where

import Network.AWS.CodeDeploy.Types.GenericRevisionInfo
import Network.AWS.CodeDeploy.Types.RevisionLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an application revision.
--
-- /See:/ 'newRevisionInfo' smart constructor.
data RevisionInfo = RevisionInfo'
  { -- | Information about an application revision, including usage details and
    -- associated deployment groups.
    genericRevisionInfo :: Prelude.Maybe GenericRevisionInfo,
    -- | Information about the location and type of an application revision.
    revisionLocation :: Prelude.Maybe RevisionLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RevisionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'genericRevisionInfo', 'revisionInfo_genericRevisionInfo' - Information about an application revision, including usage details and
-- associated deployment groups.
--
-- 'revisionLocation', 'revisionInfo_revisionLocation' - Information about the location and type of an application revision.
newRevisionInfo ::
  RevisionInfo
newRevisionInfo =
  RevisionInfo'
    { genericRevisionInfo =
        Prelude.Nothing,
      revisionLocation = Prelude.Nothing
    }

-- | Information about an application revision, including usage details and
-- associated deployment groups.
revisionInfo_genericRevisionInfo :: Lens.Lens' RevisionInfo (Prelude.Maybe GenericRevisionInfo)
revisionInfo_genericRevisionInfo = Lens.lens (\RevisionInfo' {genericRevisionInfo} -> genericRevisionInfo) (\s@RevisionInfo' {} a -> s {genericRevisionInfo = a} :: RevisionInfo)

-- | Information about the location and type of an application revision.
revisionInfo_revisionLocation :: Lens.Lens' RevisionInfo (Prelude.Maybe RevisionLocation)
revisionInfo_revisionLocation = Lens.lens (\RevisionInfo' {revisionLocation} -> revisionLocation) (\s@RevisionInfo' {} a -> s {revisionLocation = a} :: RevisionInfo)

instance Prelude.FromJSON RevisionInfo where
  parseJSON =
    Prelude.withObject
      "RevisionInfo"
      ( \x ->
          RevisionInfo'
            Prelude.<$> (x Prelude..:? "genericRevisionInfo")
            Prelude.<*> (x Prelude..:? "revisionLocation")
      )

instance Prelude.Hashable RevisionInfo

instance Prelude.NFData RevisionInfo
