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
-- Module      : Amazonka.Chime.Types.Identity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Identity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a user.
--
-- /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | The name in an Identity.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN in an Identity.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Identity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'identity_name' - The name in an Identity.
--
-- 'arn', 'identity_arn' - The ARN in an Identity.
newIdentity ::
  Identity
newIdentity =
  Identity'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The name in an Identity.
identity_name :: Lens.Lens' Identity (Prelude.Maybe Prelude.Text)
identity_name = Lens.lens (\Identity' {name} -> name) (\s@Identity' {} a -> s {name = a} :: Identity) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN in an Identity.
identity_arn :: Lens.Lens' Identity (Prelude.Maybe Prelude.Text)
identity_arn = Lens.lens (\Identity' {arn} -> arn) (\s@Identity' {} a -> s {arn = a} :: Identity)

instance Data.FromJSON Identity where
  parseJSON =
    Data.withObject
      "Identity"
      ( \x ->
          Identity'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Arn")
      )

instance Prelude.Hashable Identity where
  hashWithSalt _salt Identity' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData Identity where
  rnf Identity' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn
