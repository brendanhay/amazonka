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
-- Module      : Network.AWS.Connect.Types.UseCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UseCase where

import Network.AWS.Connect.Types.UseCaseType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the use case.
--
-- /See:/ 'newUseCase' smart constructor.
data UseCase = UseCase'
  { -- | The Amazon Resource Name (ARN) for the use case.
    useCaseArn :: Prelude.Maybe Prelude.Text,
    -- | The type of use case to associate to the AppIntegration association.
    -- Each AppIntegration association can have only one of each use case type.
    useCaseType :: Prelude.Maybe UseCaseType,
    -- | The identifier for the use case.
    useCaseId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UseCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useCaseArn', 'useCase_useCaseArn' - The Amazon Resource Name (ARN) for the use case.
--
-- 'useCaseType', 'useCase_useCaseType' - The type of use case to associate to the AppIntegration association.
-- Each AppIntegration association can have only one of each use case type.
--
-- 'useCaseId', 'useCase_useCaseId' - The identifier for the use case.
newUseCase ::
  UseCase
newUseCase =
  UseCase'
    { useCaseArn = Prelude.Nothing,
      useCaseType = Prelude.Nothing,
      useCaseId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the use case.
useCase_useCaseArn :: Lens.Lens' UseCase (Prelude.Maybe Prelude.Text)
useCase_useCaseArn = Lens.lens (\UseCase' {useCaseArn} -> useCaseArn) (\s@UseCase' {} a -> s {useCaseArn = a} :: UseCase)

-- | The type of use case to associate to the AppIntegration association.
-- Each AppIntegration association can have only one of each use case type.
useCase_useCaseType :: Lens.Lens' UseCase (Prelude.Maybe UseCaseType)
useCase_useCaseType = Lens.lens (\UseCase' {useCaseType} -> useCaseType) (\s@UseCase' {} a -> s {useCaseType = a} :: UseCase)

-- | The identifier for the use case.
useCase_useCaseId :: Lens.Lens' UseCase (Prelude.Maybe Prelude.Text)
useCase_useCaseId = Lens.lens (\UseCase' {useCaseId} -> useCaseId) (\s@UseCase' {} a -> s {useCaseId = a} :: UseCase)

instance Prelude.FromJSON UseCase where
  parseJSON =
    Prelude.withObject
      "UseCase"
      ( \x ->
          UseCase'
            Prelude.<$> (x Prelude..:? "UseCaseArn")
            Prelude.<*> (x Prelude..:? "UseCaseType")
            Prelude.<*> (x Prelude..:? "UseCaseId")
      )

instance Prelude.Hashable UseCase

instance Prelude.NFData UseCase
