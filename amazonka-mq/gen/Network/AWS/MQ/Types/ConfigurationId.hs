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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of information about the configuration.
-- Does not apply to RabbitMQ brokers.
--
-- /See:/ 'newConfigurationId' smart constructor.
data ConfigurationId = ConfigurationId'
  { -- | Required. The unique ID that Amazon MQ generates for the configuration.
    id :: Core.Maybe Core.Text,
    -- | The revision number of the configuration.
    revision :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { id = Core.Nothing,
      revision = Core.Nothing
    }

-- | Required. The unique ID that Amazon MQ generates for the configuration.
configurationId_id :: Lens.Lens' ConfigurationId (Core.Maybe Core.Text)
configurationId_id = Lens.lens (\ConfigurationId' {id} -> id) (\s@ConfigurationId' {} a -> s {id = a} :: ConfigurationId)

-- | The revision number of the configuration.
configurationId_revision :: Lens.Lens' ConfigurationId (Core.Maybe Core.Int)
configurationId_revision = Lens.lens (\ConfigurationId' {revision} -> revision) (\s@ConfigurationId' {} a -> s {revision = a} :: ConfigurationId)

instance Core.FromJSON ConfigurationId where
  parseJSON =
    Core.withObject
      "ConfigurationId"
      ( \x ->
          ConfigurationId'
            Core.<$> (x Core..:? "id") Core.<*> (x Core..:? "revision")
      )

instance Core.Hashable ConfigurationId

instance Core.NFData ConfigurationId

instance Core.ToJSON ConfigurationId where
  toJSON ConfigurationId' {..} =
    Core.object
      ( Core.catMaybes
          [ ("id" Core..=) Core.<$> id,
            ("revision" Core..=) Core.<$> revision
          ]
      )
