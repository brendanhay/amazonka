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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilterName
import qualified Amazonka.Prelude as Prelude

-- | The information for a sender ID that meets a specified criteria.
--
-- /See:/ 'newSenderIdFilter' smart constructor.
data SenderIdFilter = SenderIdFilter'
  { -- | The name of the attribute to filter on.
    name :: SenderIdFilterName,
    -- | An array of values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SenderIdFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'senderIdFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'senderIdFilter_values' - An array of values to filter for.
newSenderIdFilter ::
  -- | 'name'
  SenderIdFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  SenderIdFilter
newSenderIdFilter pName_ pValues_ =
  SenderIdFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
senderIdFilter_name :: Lens.Lens' SenderIdFilter SenderIdFilterName
senderIdFilter_name = Lens.lens (\SenderIdFilter' {name} -> name) (\s@SenderIdFilter' {} a -> s {name = a} :: SenderIdFilter)

-- | An array of values to filter for.
senderIdFilter_values :: Lens.Lens' SenderIdFilter (Prelude.NonEmpty Prelude.Text)
senderIdFilter_values = Lens.lens (\SenderIdFilter' {values} -> values) (\s@SenderIdFilter' {} a -> s {values = a} :: SenderIdFilter) Prelude.. Lens.coerced

instance Prelude.Hashable SenderIdFilter where
  hashWithSalt _salt SenderIdFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData SenderIdFilter where
  rnf SenderIdFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SenderIdFilter where
  toJSON SenderIdFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
