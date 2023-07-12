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
-- Module      : Amazonka.FIS.Types.ActionParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ActionParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a parameter for an action.
--
-- /See:/ 'newActionParameter' smart constructor.
data ActionParameter = ActionParameter'
  { -- | The parameter description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the parameter is required.
    required :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'actionParameter_description' - The parameter description.
--
-- 'required', 'actionParameter_required' - Indicates whether the parameter is required.
newActionParameter ::
  ActionParameter
newActionParameter =
  ActionParameter'
    { description = Prelude.Nothing,
      required = Prelude.Nothing
    }

-- | The parameter description.
actionParameter_description :: Lens.Lens' ActionParameter (Prelude.Maybe Prelude.Text)
actionParameter_description = Lens.lens (\ActionParameter' {description} -> description) (\s@ActionParameter' {} a -> s {description = a} :: ActionParameter)

-- | Indicates whether the parameter is required.
actionParameter_required :: Lens.Lens' ActionParameter (Prelude.Maybe Prelude.Bool)
actionParameter_required = Lens.lens (\ActionParameter' {required} -> required) (\s@ActionParameter' {} a -> s {required = a} :: ActionParameter)

instance Data.FromJSON ActionParameter where
  parseJSON =
    Data.withObject
      "ActionParameter"
      ( \x ->
          ActionParameter'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "required")
      )

instance Prelude.Hashable ActionParameter where
  hashWithSalt _salt ActionParameter' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` required

instance Prelude.NFData ActionParameter where
  rnf ActionParameter' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf required
