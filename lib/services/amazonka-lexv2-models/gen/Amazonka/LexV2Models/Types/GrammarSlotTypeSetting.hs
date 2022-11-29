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
-- Module      : Amazonka.LexV2Models.Types.GrammarSlotTypeSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.GrammarSlotTypeSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.GrammarSlotTypeSource
import qualified Amazonka.Prelude as Prelude

-- | Settings requried for a slot type based on a grammar that you provide.
--
-- /See:/ 'newGrammarSlotTypeSetting' smart constructor.
data GrammarSlotTypeSetting = GrammarSlotTypeSetting'
  { -- | The source of the grammar used to create the slot type.
    source :: Prelude.Maybe GrammarSlotTypeSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrammarSlotTypeSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'grammarSlotTypeSetting_source' - The source of the grammar used to create the slot type.
newGrammarSlotTypeSetting ::
  GrammarSlotTypeSetting
newGrammarSlotTypeSetting =
  GrammarSlotTypeSetting' {source = Prelude.Nothing}

-- | The source of the grammar used to create the slot type.
grammarSlotTypeSetting_source :: Lens.Lens' GrammarSlotTypeSetting (Prelude.Maybe GrammarSlotTypeSource)
grammarSlotTypeSetting_source = Lens.lens (\GrammarSlotTypeSetting' {source} -> source) (\s@GrammarSlotTypeSetting' {} a -> s {source = a} :: GrammarSlotTypeSetting)

instance Core.FromJSON GrammarSlotTypeSetting where
  parseJSON =
    Core.withObject
      "GrammarSlotTypeSetting"
      ( \x ->
          GrammarSlotTypeSetting'
            Prelude.<$> (x Core..:? "source")
      )

instance Prelude.Hashable GrammarSlotTypeSetting where
  hashWithSalt _salt GrammarSlotTypeSetting' {..} =
    _salt `Prelude.hashWithSalt` source

instance Prelude.NFData GrammarSlotTypeSetting where
  rnf GrammarSlotTypeSetting' {..} = Prelude.rnf source

instance Core.ToJSON GrammarSlotTypeSetting where
  toJSON GrammarSlotTypeSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [("source" Core..=) Prelude.<$> source]
      )
