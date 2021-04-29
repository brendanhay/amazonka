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
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValueSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotDefaultValueSpec where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.SlotDefaultValue
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
slotDefaultValueSpec_defaultValueList = Lens.lens (\SlotDefaultValueSpec' {defaultValueList} -> defaultValueList) (\s@SlotDefaultValueSpec' {} a -> s {defaultValueList = a} :: SlotDefaultValueSpec) Prelude.. Prelude._Coerce

instance Prelude.FromJSON SlotDefaultValueSpec where
  parseJSON =
    Prelude.withObject
      "SlotDefaultValueSpec"
      ( \x ->
          SlotDefaultValueSpec'
            Prelude.<$> ( x Prelude..:? "defaultValueList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SlotDefaultValueSpec

instance Prelude.NFData SlotDefaultValueSpec

instance Prelude.ToJSON SlotDefaultValueSpec where
  toJSON SlotDefaultValueSpec' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("defaultValueList" Prelude..= defaultValueList)
          ]
      )
