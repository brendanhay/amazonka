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
-- Module      : Amazonka.ChimeSDKMessaging.Types.MessageAttributeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.MessageAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of message attribute values.
--
-- /See:/ 'newMessageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { -- | The strings in a message attribute value.
    stringValues :: Prelude.Maybe [Core.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValues', 'messageAttributeValue_stringValues' - The strings in a message attribute value.
newMessageAttributeValue ::
  MessageAttributeValue
newMessageAttributeValue =
  MessageAttributeValue'
    { stringValues =
        Prelude.Nothing
    }

-- | The strings in a message attribute value.
messageAttributeValue_stringValues :: Lens.Lens' MessageAttributeValue (Prelude.Maybe [Prelude.Text])
messageAttributeValue_stringValues = Lens.lens (\MessageAttributeValue' {stringValues} -> stringValues) (\s@MessageAttributeValue' {} a -> s {stringValues = a} :: MessageAttributeValue) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MessageAttributeValue where
  parseJSON =
    Core.withObject
      "MessageAttributeValue"
      ( \x ->
          MessageAttributeValue'
            Prelude.<$> (x Core..:? "StringValues" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable MessageAttributeValue where
  hashWithSalt _salt MessageAttributeValue' {..} =
    _salt `Prelude.hashWithSalt` stringValues

instance Prelude.NFData MessageAttributeValue where
  rnf MessageAttributeValue' {..} =
    Prelude.rnf stringValues

instance Core.ToJSON MessageAttributeValue where
  toJSON MessageAttributeValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [("StringValues" Core..=) Prelude.<$> stringValues]
      )
