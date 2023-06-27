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
-- Module      : Amazonka.MQ.Types.ConfigurationId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.ConfigurationId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of information about the configuration.
--
-- /See:/ 'newConfigurationId' smart constructor.
data ConfigurationId = ConfigurationId'
  { -- | The revision number of the configuration.
    revision :: Prelude.Maybe Prelude.Int,
    -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revision', 'configurationId_revision' - The revision number of the configuration.
--
-- 'id', 'configurationId_id' - Required. The unique ID that Amazon MQ generates for the configuration.
newConfigurationId ::
  -- | 'id'
  Prelude.Text ->
  ConfigurationId
newConfigurationId pId_ =
  ConfigurationId'
    { revision = Prelude.Nothing,
      id = pId_
    }

-- | The revision number of the configuration.
configurationId_revision :: Lens.Lens' ConfigurationId (Prelude.Maybe Prelude.Int)
configurationId_revision = Lens.lens (\ConfigurationId' {revision} -> revision) (\s@ConfigurationId' {} a -> s {revision = a} :: ConfigurationId)

-- | Required. The unique ID that Amazon MQ generates for the configuration.
configurationId_id :: Lens.Lens' ConfigurationId Prelude.Text
configurationId_id = Lens.lens (\ConfigurationId' {id} -> id) (\s@ConfigurationId' {} a -> s {id = a} :: ConfigurationId)

instance Data.FromJSON ConfigurationId where
  parseJSON =
    Data.withObject
      "ConfigurationId"
      ( \x ->
          ConfigurationId'
            Prelude.<$> (x Data..:? "revision")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable ConfigurationId where
  hashWithSalt _salt ConfigurationId' {..} =
    _salt
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` id

instance Prelude.NFData ConfigurationId where
  rnf ConfigurationId' {..} =
    Prelude.rnf revision `Prelude.seq` Prelude.rnf id

instance Data.ToJSON ConfigurationId where
  toJSON ConfigurationId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("revision" Data..=) Prelude.<$> revision,
            Prelude.Just ("id" Data..= id)
          ]
      )
