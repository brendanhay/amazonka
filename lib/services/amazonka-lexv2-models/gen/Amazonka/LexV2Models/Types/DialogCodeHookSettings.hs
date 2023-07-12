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
-- Module      : Amazonka.LexV2Models.Types.DialogCodeHookSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DialogCodeHookSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings that determine the Lambda function that Amazon Lex uses for
-- processing user responses.
--
-- /See:/ 'newDialogCodeHookSettings' smart constructor.
data DialogCodeHookSettings = DialogCodeHookSettings'
  { -- | Enables the dialog code hook so that it processes user requests.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DialogCodeHookSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'dialogCodeHookSettings_enabled' - Enables the dialog code hook so that it processes user requests.
newDialogCodeHookSettings ::
  -- | 'enabled'
  Prelude.Bool ->
  DialogCodeHookSettings
newDialogCodeHookSettings pEnabled_ =
  DialogCodeHookSettings' {enabled = pEnabled_}

-- | Enables the dialog code hook so that it processes user requests.
dialogCodeHookSettings_enabled :: Lens.Lens' DialogCodeHookSettings Prelude.Bool
dialogCodeHookSettings_enabled = Lens.lens (\DialogCodeHookSettings' {enabled} -> enabled) (\s@DialogCodeHookSettings' {} a -> s {enabled = a} :: DialogCodeHookSettings)

instance Data.FromJSON DialogCodeHookSettings where
  parseJSON =
    Data.withObject
      "DialogCodeHookSettings"
      ( \x ->
          DialogCodeHookSettings'
            Prelude.<$> (x Data..: "enabled")
      )

instance Prelude.Hashable DialogCodeHookSettings where
  hashWithSalt _salt DialogCodeHookSettings' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData DialogCodeHookSettings where
  rnf DialogCodeHookSettings' {..} = Prelude.rnf enabled

instance Data.ToJSON DialogCodeHookSettings where
  toJSON DialogCodeHookSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("enabled" Data..= enabled)]
      )
