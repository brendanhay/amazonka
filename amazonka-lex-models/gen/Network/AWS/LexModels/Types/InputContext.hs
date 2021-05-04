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
-- Module      : Network.AWS.LexModels.Types.InputContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.InputContext where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The name of a context that must be active for an intent to be selected
-- by Amazon Lex.
--
-- /See:/ 'newInputContext' smart constructor.
data InputContext = InputContext'
  { -- | The name of the context.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON InputContext where
  parseJSON =
    Prelude.withObject
      "InputContext"
      ( \x ->
          InputContext' Prelude.<$> (x Prelude..: "name")
      )

instance Prelude.Hashable InputContext

instance Prelude.NFData InputContext

instance Prelude.ToJSON InputContext where
  toJSON InputContext' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )
