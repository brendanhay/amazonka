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
-- Module      : Amazonka.LexV2Models.Types.ExternalSourceSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ExternalSourceSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.GrammarSlotTypeSetting
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the external source of the slot type\'s
-- definition.
--
-- /See:/ 'newExternalSourceSetting' smart constructor.
data ExternalSourceSetting = ExternalSourceSetting'
  { -- | Settings required for a slot type based on a grammar that you provide.
    grammarSlotTypeSetting :: Prelude.Maybe GrammarSlotTypeSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalSourceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grammarSlotTypeSetting', 'externalSourceSetting_grammarSlotTypeSetting' - Settings required for a slot type based on a grammar that you provide.
newExternalSourceSetting ::
  ExternalSourceSetting
newExternalSourceSetting =
  ExternalSourceSetting'
    { grammarSlotTypeSetting =
        Prelude.Nothing
    }

-- | Settings required for a slot type based on a grammar that you provide.
externalSourceSetting_grammarSlotTypeSetting :: Lens.Lens' ExternalSourceSetting (Prelude.Maybe GrammarSlotTypeSetting)
externalSourceSetting_grammarSlotTypeSetting = Lens.lens (\ExternalSourceSetting' {grammarSlotTypeSetting} -> grammarSlotTypeSetting) (\s@ExternalSourceSetting' {} a -> s {grammarSlotTypeSetting = a} :: ExternalSourceSetting)

instance Data.FromJSON ExternalSourceSetting where
  parseJSON =
    Data.withObject
      "ExternalSourceSetting"
      ( \x ->
          ExternalSourceSetting'
            Prelude.<$> (x Data..:? "grammarSlotTypeSetting")
      )

instance Prelude.Hashable ExternalSourceSetting where
  hashWithSalt _salt ExternalSourceSetting' {..} =
    _salt `Prelude.hashWithSalt` grammarSlotTypeSetting

instance Prelude.NFData ExternalSourceSetting where
  rnf ExternalSourceSetting' {..} =
    Prelude.rnf grammarSlotTypeSetting

instance Data.ToJSON ExternalSourceSetting where
  toJSON ExternalSourceSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("grammarSlotTypeSetting" Data..=)
              Prelude.<$> grammarSlotTypeSetting
          ]
      )
