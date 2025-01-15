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
-- Module      : Amazonka.APIGateway.Types.StageKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.StageKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference to a unique stage identified in the format
-- @{restApiId}\/{stage}@.
--
-- /See:/ 'newStageKey' smart constructor.
data StageKey = StageKey'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Maybe Prelude.Text,
    -- | The stage name associated with the stage key.
    stageName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'stageKey_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'stageKey_stageName' - The stage name associated with the stage key.
newStageKey ::
  StageKey
newStageKey =
  StageKey'
    { restApiId = Prelude.Nothing,
      stageName = Prelude.Nothing
    }

-- | The string identifier of the associated RestApi.
stageKey_restApiId :: Lens.Lens' StageKey (Prelude.Maybe Prelude.Text)
stageKey_restApiId = Lens.lens (\StageKey' {restApiId} -> restApiId) (\s@StageKey' {} a -> s {restApiId = a} :: StageKey)

-- | The stage name associated with the stage key.
stageKey_stageName :: Lens.Lens' StageKey (Prelude.Maybe Prelude.Text)
stageKey_stageName = Lens.lens (\StageKey' {stageName} -> stageName) (\s@StageKey' {} a -> s {stageName = a} :: StageKey)

instance Prelude.Hashable StageKey where
  hashWithSalt _salt StageKey' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData StageKey where
  rnf StageKey' {..} =
    Prelude.rnf restApiId `Prelude.seq`
      Prelude.rnf stageName

instance Data.ToJSON StageKey where
  toJSON StageKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("restApiId" Data..=) Prelude.<$> restApiId,
            ("stageName" Data..=) Prelude.<$> stageName
          ]
      )
