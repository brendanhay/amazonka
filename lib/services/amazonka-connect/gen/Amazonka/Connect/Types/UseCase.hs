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
-- Module      : Amazonka.Connect.Types.UseCase
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UseCase where

import Amazonka.Connect.Types.UseCaseType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the use case.
--
-- /See:/ 'newUseCase' smart constructor.
data UseCase = UseCase'
  { -- | The type of use case to associate to the integration association. Each
    -- integration association can have only one of each use case type.
    useCaseType :: Prelude.Maybe UseCaseType,
    -- | The Amazon Resource Name (ARN) for the use case.
    useCaseArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the use case.
    useCaseId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UseCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useCaseType', 'useCase_useCaseType' - The type of use case to associate to the integration association. Each
-- integration association can have only one of each use case type.
--
-- 'useCaseArn', 'useCase_useCaseArn' - The Amazon Resource Name (ARN) for the use case.
--
-- 'useCaseId', 'useCase_useCaseId' - The identifier for the use case.
newUseCase ::
  UseCase
newUseCase =
  UseCase'
    { useCaseType = Prelude.Nothing,
      useCaseArn = Prelude.Nothing,
      useCaseId = Prelude.Nothing
    }

-- | The type of use case to associate to the integration association. Each
-- integration association can have only one of each use case type.
useCase_useCaseType :: Lens.Lens' UseCase (Prelude.Maybe UseCaseType)
useCase_useCaseType = Lens.lens (\UseCase' {useCaseType} -> useCaseType) (\s@UseCase' {} a -> s {useCaseType = a} :: UseCase)

-- | The Amazon Resource Name (ARN) for the use case.
useCase_useCaseArn :: Lens.Lens' UseCase (Prelude.Maybe Prelude.Text)
useCase_useCaseArn = Lens.lens (\UseCase' {useCaseArn} -> useCaseArn) (\s@UseCase' {} a -> s {useCaseArn = a} :: UseCase)

-- | The identifier for the use case.
useCase_useCaseId :: Lens.Lens' UseCase (Prelude.Maybe Prelude.Text)
useCase_useCaseId = Lens.lens (\UseCase' {useCaseId} -> useCaseId) (\s@UseCase' {} a -> s {useCaseId = a} :: UseCase)

instance Core.FromJSON UseCase where
  parseJSON =
    Core.withObject
      "UseCase"
      ( \x ->
          UseCase'
            Prelude.<$> (x Core..:? "UseCaseType")
            Prelude.<*> (x Core..:? "UseCaseArn")
            Prelude.<*> (x Core..:? "UseCaseId")
      )

instance Prelude.Hashable UseCase where
  hashWithSalt _salt UseCase' {..} =
    _salt `Prelude.hashWithSalt` useCaseType
      `Prelude.hashWithSalt` useCaseArn
      `Prelude.hashWithSalt` useCaseId

instance Prelude.NFData UseCase where
  rnf UseCase' {..} =
    Prelude.rnf useCaseType
      `Prelude.seq` Prelude.rnf useCaseArn
      `Prelude.seq` Prelude.rnf useCaseId
