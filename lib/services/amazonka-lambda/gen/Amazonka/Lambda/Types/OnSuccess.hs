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
-- Module      : Amazonka.Lambda.Types.OnSuccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.OnSuccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A destination for events that were processed successfully.
--
-- /See:/ 'newOnSuccess' smart constructor.
data OnSuccess = OnSuccess'
  { -- | The Amazon Resource Name (ARN) of the destination resource.
    destination :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnSuccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'onSuccess_destination' - The Amazon Resource Name (ARN) of the destination resource.
newOnSuccess ::
  OnSuccess
newOnSuccess =
  OnSuccess' {destination = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
onSuccess_destination :: Lens.Lens' OnSuccess (Prelude.Maybe Prelude.Text)
onSuccess_destination = Lens.lens (\OnSuccess' {destination} -> destination) (\s@OnSuccess' {} a -> s {destination = a} :: OnSuccess)

instance Data.FromJSON OnSuccess where
  parseJSON =
    Data.withObject
      "OnSuccess"
      ( \x ->
          OnSuccess' Prelude.<$> (x Data..:? "Destination")
      )

instance Prelude.Hashable OnSuccess where
  hashWithSalt _salt OnSuccess' {..} =
    _salt `Prelude.hashWithSalt` destination

instance Prelude.NFData OnSuccess where
  rnf OnSuccess' {..} = Prelude.rnf destination

instance Data.ToJSON OnSuccess where
  toJSON OnSuccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Destination" Data..=) Prelude.<$> destination]
      )
