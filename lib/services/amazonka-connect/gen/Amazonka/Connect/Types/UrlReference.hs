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
-- Module      : Amazonka.Connect.Types.UrlReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UrlReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The URL reference.
--
-- /See:/ 'newUrlReference' smart constructor.
data UrlReference = UrlReference'
  { -- | Identifier of the URL reference.
    name :: Prelude.Maybe Prelude.Text,
    -- | A valid URL.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UrlReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'urlReference_name' - Identifier of the URL reference.
--
-- 'value', 'urlReference_value' - A valid URL.
newUrlReference ::
  UrlReference
newUrlReference =
  UrlReference'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Identifier of the URL reference.
urlReference_name :: Lens.Lens' UrlReference (Prelude.Maybe Prelude.Text)
urlReference_name = Lens.lens (\UrlReference' {name} -> name) (\s@UrlReference' {} a -> s {name = a} :: UrlReference)

-- | A valid URL.
urlReference_value :: Lens.Lens' UrlReference (Prelude.Maybe Prelude.Text)
urlReference_value = Lens.lens (\UrlReference' {value} -> value) (\s@UrlReference' {} a -> s {value = a} :: UrlReference)

instance Data.FromJSON UrlReference where
  parseJSON =
    Data.withObject
      "UrlReference"
      ( \x ->
          UrlReference'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable UrlReference where
  hashWithSalt _salt UrlReference' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData UrlReference where
  rnf UrlReference' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
