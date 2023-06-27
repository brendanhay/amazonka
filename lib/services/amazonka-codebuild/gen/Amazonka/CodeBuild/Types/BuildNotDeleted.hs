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
-- Module      : Amazonka.CodeBuild.Types.BuildNotDeleted
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildNotDeleted where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a build that could not be successfully deleted.
--
-- /See:/ 'newBuildNotDeleted' smart constructor.
data BuildNotDeleted = BuildNotDeleted'
  { -- | The ID of the build that could not be successfully deleted.
    id :: Prelude.Maybe Prelude.Text,
    -- | Additional information about the build that could not be successfully
    -- deleted.
    statusCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildNotDeleted' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'buildNotDeleted_id' - The ID of the build that could not be successfully deleted.
--
-- 'statusCode', 'buildNotDeleted_statusCode' - Additional information about the build that could not be successfully
-- deleted.
newBuildNotDeleted ::
  BuildNotDeleted
newBuildNotDeleted =
  BuildNotDeleted'
    { id = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | The ID of the build that could not be successfully deleted.
buildNotDeleted_id :: Lens.Lens' BuildNotDeleted (Prelude.Maybe Prelude.Text)
buildNotDeleted_id = Lens.lens (\BuildNotDeleted' {id} -> id) (\s@BuildNotDeleted' {} a -> s {id = a} :: BuildNotDeleted)

-- | Additional information about the build that could not be successfully
-- deleted.
buildNotDeleted_statusCode :: Lens.Lens' BuildNotDeleted (Prelude.Maybe Prelude.Text)
buildNotDeleted_statusCode = Lens.lens (\BuildNotDeleted' {statusCode} -> statusCode) (\s@BuildNotDeleted' {} a -> s {statusCode = a} :: BuildNotDeleted)

instance Data.FromJSON BuildNotDeleted where
  parseJSON =
    Data.withObject
      "BuildNotDeleted"
      ( \x ->
          BuildNotDeleted'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "statusCode")
      )

instance Prelude.Hashable BuildNotDeleted where
  hashWithSalt _salt BuildNotDeleted' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData BuildNotDeleted where
  rnf BuildNotDeleted' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf statusCode
