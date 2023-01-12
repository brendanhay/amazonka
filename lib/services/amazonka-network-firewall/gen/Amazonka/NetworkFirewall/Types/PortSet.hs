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
-- Module      : Amazonka.NetworkFirewall.Types.PortSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.PortSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of port ranges for use in the rules in a rule group.
--
-- /See:/ 'newPortSet' smart constructor.
data PortSet = PortSet'
  { -- | The set of port ranges.
    definition :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'portSet_definition' - The set of port ranges.
newPortSet ::
  PortSet
newPortSet = PortSet' {definition = Prelude.Nothing}

-- | The set of port ranges.
portSet_definition :: Lens.Lens' PortSet (Prelude.Maybe [Prelude.Text])
portSet_definition = Lens.lens (\PortSet' {definition} -> definition) (\s@PortSet' {} a -> s {definition = a} :: PortSet) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PortSet where
  parseJSON =
    Data.withObject
      "PortSet"
      ( \x ->
          PortSet'
            Prelude.<$> (x Data..:? "Definition" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PortSet where
  hashWithSalt _salt PortSet' {..} =
    _salt `Prelude.hashWithSalt` definition

instance Prelude.NFData PortSet where
  rnf PortSet' {..} = Prelude.rnf definition

instance Data.ToJSON PortSet where
  toJSON PortSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Definition" Data..=) Prelude.<$> definition]
      )
