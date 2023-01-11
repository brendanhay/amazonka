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
-- Module      : Amazonka.LexV2Models.Types.CustomPayload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CustomPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom response string that Amazon Lex sends to your application. You
-- define the content and structure the string.
--
-- /See:/ 'newCustomPayload' smart constructor.
data CustomPayload = CustomPayload'
  { -- | The string that is sent to your application.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'customPayload_value' - The string that is sent to your application.
newCustomPayload ::
  -- | 'value'
  Prelude.Text ->
  CustomPayload
newCustomPayload pValue_ =
  CustomPayload' {value = pValue_}

-- | The string that is sent to your application.
customPayload_value :: Lens.Lens' CustomPayload Prelude.Text
customPayload_value = Lens.lens (\CustomPayload' {value} -> value) (\s@CustomPayload' {} a -> s {value = a} :: CustomPayload)

instance Data.FromJSON CustomPayload where
  parseJSON =
    Data.withObject
      "CustomPayload"
      ( \x ->
          CustomPayload' Prelude.<$> (x Data..: "value")
      )

instance Prelude.Hashable CustomPayload where
  hashWithSalt _salt CustomPayload' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData CustomPayload where
  rnf CustomPayload' {..} = Prelude.rnf value

instance Data.ToJSON CustomPayload where
  toJSON CustomPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("value" Data..= value)]
      )
