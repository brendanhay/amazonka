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
-- Module      : Network.AWS.LexV2Models.Types.SlotDefaultValueSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.SlotDefaultValueSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.SlotDefaultValue
import qualified Network.AWS.Prelude as Prelude

-- | Defines a list of values that Amazon Lex should use as the default value
-- for a slot.
--
-- /See:/ 'newSlotDefaultValueSpecification' smart constructor.
data SlotDefaultValueSpecification = SlotDefaultValueSpecification'
  { -- | A list of default values. Amazon Lex chooses the default value to use in
    -- the order that they are presented in the list.
    defaultValueList :: [SlotDefaultValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotDefaultValueSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValueList', 'slotDefaultValueSpecification_defaultValueList' - A list of default values. Amazon Lex chooses the default value to use in
-- the order that they are presented in the list.
newSlotDefaultValueSpecification ::
  SlotDefaultValueSpecification
newSlotDefaultValueSpecification =
  SlotDefaultValueSpecification'
    { defaultValueList =
        Prelude.mempty
    }

-- | A list of default values. Amazon Lex chooses the default value to use in
-- the order that they are presented in the list.
slotDefaultValueSpecification_defaultValueList :: Lens.Lens' SlotDefaultValueSpecification [SlotDefaultValue]
slotDefaultValueSpecification_defaultValueList = Lens.lens (\SlotDefaultValueSpecification' {defaultValueList} -> defaultValueList) (\s@SlotDefaultValueSpecification' {} a -> s {defaultValueList = a} :: SlotDefaultValueSpecification) Prelude.. Lens.coerced

instance Core.FromJSON SlotDefaultValueSpecification where
  parseJSON =
    Core.withObject
      "SlotDefaultValueSpecification"
      ( \x ->
          SlotDefaultValueSpecification'
            Prelude.<$> ( x Core..:? "defaultValueList"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    SlotDefaultValueSpecification

instance Prelude.NFData SlotDefaultValueSpecification

instance Core.ToJSON SlotDefaultValueSpecification where
  toJSON SlotDefaultValueSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("defaultValueList" Core..= defaultValueList)
          ]
      )
