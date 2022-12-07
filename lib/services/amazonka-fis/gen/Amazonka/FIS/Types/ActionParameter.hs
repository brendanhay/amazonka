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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | Indicates whether the parameter is required.
    required :: Prelude.Maybe Prelude.Bool,
    -- | The parameter description.
    description :: Prelude.Maybe Prelude.Text
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
-- 'required', 'actionParameter_required' - Indicates whether the parameter is required.
--
-- 'description', 'actionParameter_description' - The parameter description.
newActionParameter ::
  ActionParameter
newActionParameter =
  ActionParameter'
    { required = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Indicates whether the parameter is required.
actionParameter_required :: Lens.Lens' ActionParameter (Prelude.Maybe Prelude.Bool)
actionParameter_required = Lens.lens (\ActionParameter' {required} -> required) (\s@ActionParameter' {} a -> s {required = a} :: ActionParameter)

-- | The parameter description.
actionParameter_description :: Lens.Lens' ActionParameter (Prelude.Maybe Prelude.Text)
actionParameter_description = Lens.lens (\ActionParameter' {description} -> description) (\s@ActionParameter' {} a -> s {description = a} :: ActionParameter)

instance Data.FromJSON ActionParameter where
  parseJSON =
    Data.withObject
      "ActionParameter"
      ( \x ->
          ActionParameter'
            Prelude.<$> (x Data..:? "required")
            Prelude.<*> (x Data..:? "description")
      )

instance Prelude.Hashable ActionParameter where
  hashWithSalt _salt ActionParameter' {..} =
    _salt `Prelude.hashWithSalt` required
      `Prelude.hashWithSalt` description

instance Prelude.NFData ActionParameter where
  rnf ActionParameter' {..} =
    Prelude.rnf required
      `Prelude.seq` Prelude.rnf description
