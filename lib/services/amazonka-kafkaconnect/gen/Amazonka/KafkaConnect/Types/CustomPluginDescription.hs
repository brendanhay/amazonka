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
-- Module      : Amazonka.KafkaConnect.Types.CustomPluginDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CustomPluginDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a custom plugin.
--
-- /See:/ 'newCustomPluginDescription' smart constructor.
data CustomPluginDescription = CustomPluginDescription'
  { -- | The Amazon Resource Name (ARN) of the custom plugin.
    customPluginArn :: Prelude.Maybe Prelude.Text,
    -- | The revision of the custom plugin.
    revision :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomPluginDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPluginArn', 'customPluginDescription_customPluginArn' - The Amazon Resource Name (ARN) of the custom plugin.
--
-- 'revision', 'customPluginDescription_revision' - The revision of the custom plugin.
newCustomPluginDescription ::
  CustomPluginDescription
newCustomPluginDescription =
  CustomPluginDescription'
    { customPluginArn =
        Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom plugin.
customPluginDescription_customPluginArn :: Lens.Lens' CustomPluginDescription (Prelude.Maybe Prelude.Text)
customPluginDescription_customPluginArn = Lens.lens (\CustomPluginDescription' {customPluginArn} -> customPluginArn) (\s@CustomPluginDescription' {} a -> s {customPluginArn = a} :: CustomPluginDescription)

-- | The revision of the custom plugin.
customPluginDescription_revision :: Lens.Lens' CustomPluginDescription (Prelude.Maybe Prelude.Integer)
customPluginDescription_revision = Lens.lens (\CustomPluginDescription' {revision} -> revision) (\s@CustomPluginDescription' {} a -> s {revision = a} :: CustomPluginDescription)

instance Data.FromJSON CustomPluginDescription where
  parseJSON =
    Data.withObject
      "CustomPluginDescription"
      ( \x ->
          CustomPluginDescription'
            Prelude.<$> (x Data..:? "customPluginArn")
            Prelude.<*> (x Data..:? "revision")
      )

instance Prelude.Hashable CustomPluginDescription where
  hashWithSalt _salt CustomPluginDescription' {..} =
    _salt
      `Prelude.hashWithSalt` customPluginArn
      `Prelude.hashWithSalt` revision

instance Prelude.NFData CustomPluginDescription where
  rnf CustomPluginDescription' {..} =
    Prelude.rnf customPluginArn
      `Prelude.seq` Prelude.rnf revision
