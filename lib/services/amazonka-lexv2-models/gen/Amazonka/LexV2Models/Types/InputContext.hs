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
-- Module      : Amazonka.LexV2Models.Types.InputContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.InputContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of a context that must be active for an intent to be selected
-- by Amazon Lex.
--
-- /See:/ 'newInputContext' smart constructor.
data InputContext = InputContext'
  { -- | The name of the context.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'inputContext_name' - The name of the context.
newInputContext ::
  -- | 'name'
  Prelude.Text ->
  InputContext
newInputContext pName_ = InputContext' {name = pName_}

-- | The name of the context.
inputContext_name :: Lens.Lens' InputContext Prelude.Text
inputContext_name = Lens.lens (\InputContext' {name} -> name) (\s@InputContext' {} a -> s {name = a} :: InputContext)

instance Data.FromJSON InputContext where
  parseJSON =
    Data.withObject
      "InputContext"
      (\x -> InputContext' Prelude.<$> (x Data..: "name"))

instance Prelude.Hashable InputContext where
  hashWithSalt _salt InputContext' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData InputContext where
  rnf InputContext' {..} = Prelude.rnf name

instance Data.ToJSON InputContext where
  toJSON InputContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )
