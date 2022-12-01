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
-- Module      : Amazonka.GroundStation.Types.ConfigIdResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConfigIdResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.ConfigCapabilityType
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newConfigIdResponse' smart constructor.
data ConfigIdResponse = ConfigIdResponse'
  { -- | UUID of a @Config@.
    configId :: Prelude.Maybe Prelude.Text,
    -- | Type of a @Config@.
    configType :: Prelude.Maybe ConfigCapabilityType,
    -- | ARN of a @Config@.
    configArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configId', 'configIdResponse_configId' - UUID of a @Config@.
--
-- 'configType', 'configIdResponse_configType' - Type of a @Config@.
--
-- 'configArn', 'configIdResponse_configArn' - ARN of a @Config@.
newConfigIdResponse ::
  ConfigIdResponse
newConfigIdResponse =
  ConfigIdResponse'
    { configId = Prelude.Nothing,
      configType = Prelude.Nothing,
      configArn = Prelude.Nothing
    }

-- | UUID of a @Config@.
configIdResponse_configId :: Lens.Lens' ConfigIdResponse (Prelude.Maybe Prelude.Text)
configIdResponse_configId = Lens.lens (\ConfigIdResponse' {configId} -> configId) (\s@ConfigIdResponse' {} a -> s {configId = a} :: ConfigIdResponse)

-- | Type of a @Config@.
configIdResponse_configType :: Lens.Lens' ConfigIdResponse (Prelude.Maybe ConfigCapabilityType)
configIdResponse_configType = Lens.lens (\ConfigIdResponse' {configType} -> configType) (\s@ConfigIdResponse' {} a -> s {configType = a} :: ConfigIdResponse)

-- | ARN of a @Config@.
configIdResponse_configArn :: Lens.Lens' ConfigIdResponse (Prelude.Maybe Prelude.Text)
configIdResponse_configArn = Lens.lens (\ConfigIdResponse' {configArn} -> configArn) (\s@ConfigIdResponse' {} a -> s {configArn = a} :: ConfigIdResponse)

instance Core.FromJSON ConfigIdResponse where
  parseJSON =
    Core.withObject
      "ConfigIdResponse"
      ( \x ->
          ConfigIdResponse'
            Prelude.<$> (x Core..:? "configId")
            Prelude.<*> (x Core..:? "configType")
            Prelude.<*> (x Core..:? "configArn")
      )

instance Prelude.Hashable ConfigIdResponse where
  hashWithSalt _salt ConfigIdResponse' {..} =
    _salt `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType
      `Prelude.hashWithSalt` configArn

instance Prelude.NFData ConfigIdResponse where
  rnf ConfigIdResponse' {..} =
    Prelude.rnf configId
      `Prelude.seq` Prelude.rnf configType
      `Prelude.seq` Prelude.rnf configArn
