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
-- Module      : Amazonka.LexV2Models.Types.ActiveContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ActiveContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The active context used in the test execution.
--
-- /See:/ 'newActiveContext' smart constructor.
data ActiveContext = ActiveContext'
  { -- | The name of active context.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'activeContext_name' - The name of active context.
newActiveContext ::
  -- | 'name'
  Prelude.Text ->
  ActiveContext
newActiveContext pName_ =
  ActiveContext' {name = pName_}

-- | The name of active context.
activeContext_name :: Lens.Lens' ActiveContext Prelude.Text
activeContext_name = Lens.lens (\ActiveContext' {name} -> name) (\s@ActiveContext' {} a -> s {name = a} :: ActiveContext)

instance Data.FromJSON ActiveContext where
  parseJSON =
    Data.withObject
      "ActiveContext"
      ( \x ->
          ActiveContext' Prelude.<$> (x Data..: "name")
      )

instance Prelude.Hashable ActiveContext where
  hashWithSalt _salt ActiveContext' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ActiveContext where
  rnf ActiveContext' {..} = Prelude.rnf name
