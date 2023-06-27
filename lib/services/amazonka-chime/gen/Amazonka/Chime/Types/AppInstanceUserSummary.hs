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
-- Module      : Amazonka.Chime.Types.AppInstanceUserSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AppInstanceUserSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of an @AppInstanceUser@.
--
-- /See:/ 'newAppInstanceUserSummary' smart constructor.
data AppInstanceUserSummary = AppInstanceUserSummary'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the @AppInstanceUser@.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of an @AppInstanceUser@.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceUserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'appInstanceUserSummary_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'metadata', 'appInstanceUserSummary_metadata' - The metadata of the @AppInstanceUser@.
--
-- 'name', 'appInstanceUserSummary_name' - The name of an @AppInstanceUser@.
newAppInstanceUserSummary ::
  AppInstanceUserSummary
newAppInstanceUserSummary =
  AppInstanceUserSummary'
    { appInstanceUserArn =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the @AppInstanceUser@.
appInstanceUserSummary_appInstanceUserArn :: Lens.Lens' AppInstanceUserSummary (Prelude.Maybe Prelude.Text)
appInstanceUserSummary_appInstanceUserArn = Lens.lens (\AppInstanceUserSummary' {appInstanceUserArn} -> appInstanceUserArn) (\s@AppInstanceUserSummary' {} a -> s {appInstanceUserArn = a} :: AppInstanceUserSummary)

-- | The metadata of the @AppInstanceUser@.
appInstanceUserSummary_metadata :: Lens.Lens' AppInstanceUserSummary (Prelude.Maybe Prelude.Text)
appInstanceUserSummary_metadata = Lens.lens (\AppInstanceUserSummary' {metadata} -> metadata) (\s@AppInstanceUserSummary' {} a -> s {metadata = a} :: AppInstanceUserSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The name of an @AppInstanceUser@.
appInstanceUserSummary_name :: Lens.Lens' AppInstanceUserSummary (Prelude.Maybe Prelude.Text)
appInstanceUserSummary_name = Lens.lens (\AppInstanceUserSummary' {name} -> name) (\s@AppInstanceUserSummary' {} a -> s {name = a} :: AppInstanceUserSummary) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON AppInstanceUserSummary where
  parseJSON =
    Data.withObject
      "AppInstanceUserSummary"
      ( \x ->
          AppInstanceUserSummary'
            Prelude.<$> (x Data..:? "AppInstanceUserArn")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AppInstanceUserSummary where
  hashWithSalt _salt AppInstanceUserSummary' {..} =
    _salt
      `Prelude.hashWithSalt` appInstanceUserArn
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name

instance Prelude.NFData AppInstanceUserSummary where
  rnf AppInstanceUserSummary' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
