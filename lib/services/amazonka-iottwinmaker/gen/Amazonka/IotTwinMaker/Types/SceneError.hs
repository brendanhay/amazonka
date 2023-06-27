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
-- Module      : Amazonka.IotTwinMaker.Types.SceneError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SceneError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.SceneErrorCode
import qualified Amazonka.Prelude as Prelude

-- | The scene error.
--
-- /See:/ 'newSceneError' smart constructor.
data SceneError = SceneError'
  { -- | The SceneError code.
    code :: Prelude.Maybe SceneErrorCode,
    -- | The SceneError message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SceneError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'sceneError_code' - The SceneError code.
--
-- 'message', 'sceneError_message' - The SceneError message.
newSceneError ::
  SceneError
newSceneError =
  SceneError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The SceneError code.
sceneError_code :: Lens.Lens' SceneError (Prelude.Maybe SceneErrorCode)
sceneError_code = Lens.lens (\SceneError' {code} -> code) (\s@SceneError' {} a -> s {code = a} :: SceneError)

-- | The SceneError message.
sceneError_message :: Lens.Lens' SceneError (Prelude.Maybe Prelude.Text)
sceneError_message = Lens.lens (\SceneError' {message} -> message) (\s@SceneError' {} a -> s {message = a} :: SceneError)

instance Data.FromJSON SceneError where
  parseJSON =
    Data.withObject
      "SceneError"
      ( \x ->
          SceneError'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable SceneError where
  hashWithSalt _salt SceneError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData SceneError where
  rnf SceneError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
