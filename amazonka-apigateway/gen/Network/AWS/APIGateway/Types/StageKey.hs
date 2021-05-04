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
-- Module      : Network.AWS.APIGateway.Types.StageKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.StageKey where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A reference to a unique stage identified in the format
-- @{restApiId}\/{stage}@.
--
-- /See:/ 'newStageKey' smart constructor.
data StageKey = StageKey'
  { -- | The stage name associated with the stage key.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StageKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'stageKey_stageName' - The stage name associated with the stage key.
--
-- 'restApiId', 'stageKey_restApiId' - The string identifier of the associated RestApi.
newStageKey ::
  StageKey
newStageKey =
  StageKey'
    { stageName = Prelude.Nothing,
      restApiId = Prelude.Nothing
    }

-- | The stage name associated with the stage key.
stageKey_stageName :: Lens.Lens' StageKey (Prelude.Maybe Prelude.Text)
stageKey_stageName = Lens.lens (\StageKey' {stageName} -> stageName) (\s@StageKey' {} a -> s {stageName = a} :: StageKey)

-- | The string identifier of the associated RestApi.
stageKey_restApiId :: Lens.Lens' StageKey (Prelude.Maybe Prelude.Text)
stageKey_restApiId = Lens.lens (\StageKey' {restApiId} -> restApiId) (\s@StageKey' {} a -> s {restApiId = a} :: StageKey)

instance Prelude.Hashable StageKey

instance Prelude.NFData StageKey

instance Prelude.ToJSON StageKey where
  toJSON StageKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("stageName" Prelude..=) Prelude.<$> stageName,
            ("restApiId" Prelude..=) Prelude.<$> restApiId
          ]
      )
