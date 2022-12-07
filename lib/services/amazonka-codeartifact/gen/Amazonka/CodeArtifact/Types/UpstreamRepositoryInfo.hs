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
-- Module      : Amazonka.CodeArtifact.Types.UpstreamRepositoryInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.UpstreamRepositoryInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an upstream repository.
--
-- /See:/ 'newUpstreamRepositoryInfo' smart constructor.
data UpstreamRepositoryInfo = UpstreamRepositoryInfo'
  { -- | The name of an upstream repository.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpstreamRepositoryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'upstreamRepositoryInfo_repositoryName' - The name of an upstream repository.
newUpstreamRepositoryInfo ::
  UpstreamRepositoryInfo
newUpstreamRepositoryInfo =
  UpstreamRepositoryInfo'
    { repositoryName =
        Prelude.Nothing
    }

-- | The name of an upstream repository.
upstreamRepositoryInfo_repositoryName :: Lens.Lens' UpstreamRepositoryInfo (Prelude.Maybe Prelude.Text)
upstreamRepositoryInfo_repositoryName = Lens.lens (\UpstreamRepositoryInfo' {repositoryName} -> repositoryName) (\s@UpstreamRepositoryInfo' {} a -> s {repositoryName = a} :: UpstreamRepositoryInfo)

instance Data.FromJSON UpstreamRepositoryInfo where
  parseJSON =
    Data.withObject
      "UpstreamRepositoryInfo"
      ( \x ->
          UpstreamRepositoryInfo'
            Prelude.<$> (x Data..:? "repositoryName")
      )

instance Prelude.Hashable UpstreamRepositoryInfo where
  hashWithSalt _salt UpstreamRepositoryInfo' {..} =
    _salt `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData UpstreamRepositoryInfo where
  rnf UpstreamRepositoryInfo' {..} =
    Prelude.rnf repositoryName
