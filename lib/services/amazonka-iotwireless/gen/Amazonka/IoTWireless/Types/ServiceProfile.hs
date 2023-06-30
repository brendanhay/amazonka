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
-- Module      : Amazonka.IoTWireless.Types.ServiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ServiceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a service profile.
--
-- /See:/ 'newServiceProfile' smart constructor.
data ServiceProfile = ServiceProfile'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'serviceProfile_arn' - The Amazon Resource Name of the resource.
--
-- 'id', 'serviceProfile_id' - The ID of the service profile.
--
-- 'name', 'serviceProfile_name' - The name of the resource.
newServiceProfile ::
  ServiceProfile
newServiceProfile =
  ServiceProfile'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name of the resource.
serviceProfile_arn :: Lens.Lens' ServiceProfile (Prelude.Maybe Prelude.Text)
serviceProfile_arn = Lens.lens (\ServiceProfile' {arn} -> arn) (\s@ServiceProfile' {} a -> s {arn = a} :: ServiceProfile)

-- | The ID of the service profile.
serviceProfile_id :: Lens.Lens' ServiceProfile (Prelude.Maybe Prelude.Text)
serviceProfile_id = Lens.lens (\ServiceProfile' {id} -> id) (\s@ServiceProfile' {} a -> s {id = a} :: ServiceProfile)

-- | The name of the resource.
serviceProfile_name :: Lens.Lens' ServiceProfile (Prelude.Maybe Prelude.Text)
serviceProfile_name = Lens.lens (\ServiceProfile' {name} -> name) (\s@ServiceProfile' {} a -> s {name = a} :: ServiceProfile)

instance Data.FromJSON ServiceProfile where
  parseJSON =
    Data.withObject
      "ServiceProfile"
      ( \x ->
          ServiceProfile'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ServiceProfile where
  hashWithSalt _salt ServiceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ServiceProfile where
  rnf ServiceProfile' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
