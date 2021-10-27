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
-- Module      : Network.AWS.Kafka.Types.ConfigurationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kafka.Types.ConfigurationInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration to use for the brokers.
--
-- /See:/ 'newConfigurationInfo' smart constructor.
data ConfigurationInfo = ConfigurationInfo'
  { -- | The revision of the configuration to use.
    revision :: Prelude.Integer,
    -- | ARN of the configuration to use.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revision', 'configurationInfo_revision' - The revision of the configuration to use.
--
-- 'arn', 'configurationInfo_arn' - ARN of the configuration to use.
newConfigurationInfo ::
  -- | 'revision'
  Prelude.Integer ->
  -- | 'arn'
  Prelude.Text ->
  ConfigurationInfo
newConfigurationInfo pRevision_ pArn_ =
  ConfigurationInfo'
    { revision = pRevision_,
      arn = pArn_
    }

-- | The revision of the configuration to use.
configurationInfo_revision :: Lens.Lens' ConfigurationInfo Prelude.Integer
configurationInfo_revision = Lens.lens (\ConfigurationInfo' {revision} -> revision) (\s@ConfigurationInfo' {} a -> s {revision = a} :: ConfigurationInfo)

-- | ARN of the configuration to use.
configurationInfo_arn :: Lens.Lens' ConfigurationInfo Prelude.Text
configurationInfo_arn = Lens.lens (\ConfigurationInfo' {arn} -> arn) (\s@ConfigurationInfo' {} a -> s {arn = a} :: ConfigurationInfo)

instance Core.FromJSON ConfigurationInfo where
  parseJSON =
    Core.withObject
      "ConfigurationInfo"
      ( \x ->
          ConfigurationInfo'
            Prelude.<$> (x Core..: "revision") Prelude.<*> (x Core..: "arn")
      )

instance Prelude.Hashable ConfigurationInfo

instance Prelude.NFData ConfigurationInfo

instance Core.ToJSON ConfigurationInfo where
  toJSON ConfigurationInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("revision" Core..= revision),
            Prelude.Just ("arn" Core..= arn)
          ]
      )
