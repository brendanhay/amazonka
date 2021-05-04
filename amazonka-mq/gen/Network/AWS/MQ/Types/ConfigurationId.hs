{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MQ.Types.ConfigurationId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ConfigurationId where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of information about the configuration.
-- Does not apply to RabbitMQ brokers.
--
-- /See:/ 'newConfigurationId' smart constructor.
data ConfigurationId = ConfigurationId'
  { -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Prelude.Maybe Prelude.Text,
    -- | The revision number of the configuration.
    revision :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'configurationId_id' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- 'revision', 'configurationId_revision' - The revision number of the configuration.
newConfigurationId ::
  ConfigurationId
newConfigurationId =
  ConfigurationId'
    { id = Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | Required. The unique ID that Amazon MQ generates for the configuration.
configurationId_id :: Lens.Lens' ConfigurationId (Prelude.Maybe Prelude.Text)
configurationId_id = Lens.lens (\ConfigurationId' {id} -> id) (\s@ConfigurationId' {} a -> s {id = a} :: ConfigurationId)

-- | The revision number of the configuration.
configurationId_revision :: Lens.Lens' ConfigurationId (Prelude.Maybe Prelude.Int)
configurationId_revision = Lens.lens (\ConfigurationId' {revision} -> revision) (\s@ConfigurationId' {} a -> s {revision = a} :: ConfigurationId)

instance Prelude.FromJSON ConfigurationId where
  parseJSON =
    Prelude.withObject
      "ConfigurationId"
      ( \x ->
          ConfigurationId'
            Prelude.<$> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "revision")
      )

instance Prelude.Hashable ConfigurationId

instance Prelude.NFData ConfigurationId

instance Prelude.ToJSON ConfigurationId where
  toJSON ConfigurationId' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("id" Prelude..=) Prelude.<$> id,
            ("revision" Prelude..=) Prelude.<$> revision
          ]
      )
