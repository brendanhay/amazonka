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
-- Module      : Amazonka.LexModels.Types.SlotDefaultValueSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.SlotDefaultValueSpec where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.SlotDefaultValue
import qualified Amazonka.Prelude as Prelude

-- | Contains the default values for a slot. Default values are used when
-- Amazon Lex hasn\'t determined a value for a slot.
--
-- /See:/ 'newSlotDefaultValueSpec' smart constructor.
data SlotDefaultValueSpec = SlotDefaultValueSpec'
  { -- | The default values for a slot. You can specify more than one default.
    -- For example, you can specify a default value to use from a matching
    -- context variable, a session attribute, or a fixed value.
    --
    -- The default value chosen is selected based on the order that you specify
    -- them in the list. For example, if you specify a context variable and a
    -- fixed value in that order, Amazon Lex uses the context variable if it is
    -- available, else it uses the fixed value.
    defaultValueList :: [SlotDefaultValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotDefaultValueSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValueList', 'slotDefaultValueSpec_defaultValueList' - The default values for a slot. You can specify more than one default.
-- For example, you can specify a default value to use from a matching
-- context variable, a session attribute, or a fixed value.
--
-- The default value chosen is selected based on the order that you specify
-- them in the list. For example, if you specify a context variable and a
-- fixed value in that order, Amazon Lex uses the context variable if it is
-- available, else it uses the fixed value.
newSlotDefaultValueSpec ::
  SlotDefaultValueSpec
newSlotDefaultValueSpec =
  SlotDefaultValueSpec'
    { defaultValueList =
        Prelude.mempty
    }

-- | The default values for a slot. You can specify more than one default.
-- For example, you can specify a default value to use from a matching
-- context variable, a session attribute, or a fixed value.
--
-- The default value chosen is selected based on the order that you specify
-- them in the list. For example, if you specify a context variable and a
-- fixed value in that order, Amazon Lex uses the context variable if it is
-- available, else it uses the fixed value.
slotDefaultValueSpec_defaultValueList :: Lens.Lens' SlotDefaultValueSpec [SlotDefaultValue]
slotDefaultValueSpec_defaultValueList = Lens.lens (\SlotDefaultValueSpec' {defaultValueList} -> defaultValueList) (\s@SlotDefaultValueSpec' {} a -> s {defaultValueList = a} :: SlotDefaultValueSpec) Prelude.. Lens.coerced

instance Data.FromJSON SlotDefaultValueSpec where
  parseJSON =
    Data.withObject
      "SlotDefaultValueSpec"
      ( \x ->
          SlotDefaultValueSpec'
            Prelude.<$> ( x
                            Data..:? "defaultValueList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SlotDefaultValueSpec where
  hashWithSalt _salt SlotDefaultValueSpec' {..} =
    _salt `Prelude.hashWithSalt` defaultValueList

instance Prelude.NFData SlotDefaultValueSpec where
  rnf SlotDefaultValueSpec' {..} =
    Prelude.rnf defaultValueList

instance Data.ToJSON SlotDefaultValueSpec where
  toJSON SlotDefaultValueSpec' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("defaultValueList" Data..= defaultValueList)
          ]
      )
