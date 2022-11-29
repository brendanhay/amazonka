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
-- Module      : Amazonka.LexV2Models.Types.ObfuscationSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ObfuscationSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.ObfuscationSettingType
import qualified Amazonka.Prelude as Prelude

-- | Determines whether Amazon Lex obscures slot values in conversation logs.
--
-- /See:/ 'newObfuscationSetting' smart constructor.
data ObfuscationSetting = ObfuscationSetting'
  { -- | Value that determines whether Amazon Lex obscures slot values in
    -- conversation logs. The default is to obscure the values.
    obfuscationSettingType :: ObfuscationSettingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObfuscationSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'obfuscationSettingType', 'obfuscationSetting_obfuscationSettingType' - Value that determines whether Amazon Lex obscures slot values in
-- conversation logs. The default is to obscure the values.
newObfuscationSetting ::
  -- | 'obfuscationSettingType'
  ObfuscationSettingType ->
  ObfuscationSetting
newObfuscationSetting pObfuscationSettingType_ =
  ObfuscationSetting'
    { obfuscationSettingType =
        pObfuscationSettingType_
    }

-- | Value that determines whether Amazon Lex obscures slot values in
-- conversation logs. The default is to obscure the values.
obfuscationSetting_obfuscationSettingType :: Lens.Lens' ObfuscationSetting ObfuscationSettingType
obfuscationSetting_obfuscationSettingType = Lens.lens (\ObfuscationSetting' {obfuscationSettingType} -> obfuscationSettingType) (\s@ObfuscationSetting' {} a -> s {obfuscationSettingType = a} :: ObfuscationSetting)

instance Core.FromJSON ObfuscationSetting where
  parseJSON =
    Core.withObject
      "ObfuscationSetting"
      ( \x ->
          ObfuscationSetting'
            Prelude.<$> (x Core..: "obfuscationSettingType")
      )

instance Prelude.Hashable ObfuscationSetting where
  hashWithSalt _salt ObfuscationSetting' {..} =
    _salt `Prelude.hashWithSalt` obfuscationSettingType

instance Prelude.NFData ObfuscationSetting where
  rnf ObfuscationSetting' {..} =
    Prelude.rnf obfuscationSettingType

instance Core.ToJSON ObfuscationSetting where
  toJSON ObfuscationSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "obfuscationSettingType"
                  Core..= obfuscationSettingType
              )
          ]
      )
