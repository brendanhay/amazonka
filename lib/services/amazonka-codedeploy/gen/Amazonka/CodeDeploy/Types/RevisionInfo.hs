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
-- Module      : Amazonka.CodeDeploy.Types.RevisionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.RevisionInfo where

import Amazonka.CodeDeploy.Types.GenericRevisionInfo
import Amazonka.CodeDeploy.Types.RevisionLocation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON RevisionInfo where
  parseJSON =
    Data.withObject
      "RevisionInfo"
      ( \x ->
          RevisionInfo'
            Prelude.<$> (x Data..:? "genericRevisionInfo")
            Prelude.<*> (x Data..:? "revisionLocation")
      )

instance Prelude.Hashable RevisionInfo where
  hashWithSalt _salt RevisionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` genericRevisionInfo
      `Prelude.hashWithSalt` revisionLocation

instance Prelude.NFData RevisionInfo where
  rnf RevisionInfo' {..} =
    Prelude.rnf genericRevisionInfo `Prelude.seq`
      Prelude.rnf revisionLocation
