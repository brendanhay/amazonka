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
-- Module      : Amazonka.CodeStar.Types.Code
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.Code where

import Amazonka.CodeStar.Types.CodeDestination
import Amazonka.CodeStar.Types.CodeSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Location and destination information about the source code files
-- provided with the project request. The source code is uploaded to the
-- new project source repository after project creation.
--
-- /See:/ 'newCode' smart constructor.
data Code = Code'
  { -- | The location where the source code files provided with the project
    -- request are stored. AWS CodeStar retrieves the files during project
    -- creation.
    source :: CodeSource,
    -- | The repository to be created in AWS CodeStar. Valid values are AWS
    -- CodeCommit or GitHub. After AWS CodeStar provisions the new repository,
    -- the source code files provided with the project request are placed in
    -- the repository.
    destination :: CodeDestination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Code' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'code_source' - The location where the source code files provided with the project
-- request are stored. AWS CodeStar retrieves the files during project
-- creation.
--
-- 'destination', 'code_destination' - The repository to be created in AWS CodeStar. Valid values are AWS
-- CodeCommit or GitHub. After AWS CodeStar provisions the new repository,
-- the source code files provided with the project request are placed in
-- the repository.
newCode ::
  -- | 'source'
  CodeSource ->
  -- | 'destination'
  CodeDestination ->
  Code
newCode pSource_ pDestination_ =
  Code'
    { source = pSource_,
      destination = pDestination_
    }

-- | The location where the source code files provided with the project
-- request are stored. AWS CodeStar retrieves the files during project
-- creation.
code_source :: Lens.Lens' Code CodeSource
code_source = Lens.lens (\Code' {source} -> source) (\s@Code' {} a -> s {source = a} :: Code)

-- | The repository to be created in AWS CodeStar. Valid values are AWS
-- CodeCommit or GitHub. After AWS CodeStar provisions the new repository,
-- the source code files provided with the project request are placed in
-- the repository.
code_destination :: Lens.Lens' Code CodeDestination
code_destination = Lens.lens (\Code' {destination} -> destination) (\s@Code' {} a -> s {destination = a} :: Code)

instance Prelude.Hashable Code where
  hashWithSalt _salt Code' {..} =
    _salt
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` destination

instance Prelude.NFData Code where
  rnf Code' {..} =
    Prelude.rnf source `Prelude.seq`
      Prelude.rnf destination

instance Data.ToJSON Code where
  toJSON Code' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("source" Data..= source),
            Prelude.Just ("destination" Data..= destination)
          ]
      )
