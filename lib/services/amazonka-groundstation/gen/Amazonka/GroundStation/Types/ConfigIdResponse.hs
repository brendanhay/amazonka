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
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.ConfigCapabilityType
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newConfigIdResponse' smart constructor.
data ConfigIdResponse = ConfigIdResponse'
  { -- | ARN of a @Config@.
    configArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a @Config@.
    configId :: Prelude.Maybe Prelude.Text,
    -- | Type of a @Config@.
    configType :: Prelude.Maybe ConfigCapabilityType
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
-- 'configArn', 'configIdResponse_configArn' - ARN of a @Config@.
--
-- 'configId', 'configIdResponse_configId' - UUID of a @Config@.
--
-- 'configType', 'configIdResponse_configType' - Type of a @Config@.
newConfigIdResponse ::
  ConfigIdResponse
newConfigIdResponse =
  ConfigIdResponse'
    { configArn = Prelude.Nothing,
      configId = Prelude.Nothing,
      configType = Prelude.Nothing
    }

-- | ARN of a @Config@.
configIdResponse_configArn :: Lens.Lens' ConfigIdResponse (Prelude.Maybe Prelude.Text)
configIdResponse_configArn = Lens.lens (\ConfigIdResponse' {configArn} -> configArn) (\s@ConfigIdResponse' {} a -> s {configArn = a} :: ConfigIdResponse)

-- | UUID of a @Config@.
configIdResponse_configId :: Lens.Lens' ConfigIdResponse (Prelude.Maybe Prelude.Text)
configIdResponse_configId = Lens.lens (\ConfigIdResponse' {configId} -> configId) (\s@ConfigIdResponse' {} a -> s {configId = a} :: ConfigIdResponse)

-- | Type of a @Config@.
configIdResponse_configType :: Lens.Lens' ConfigIdResponse (Prelude.Maybe ConfigCapabilityType)
configIdResponse_configType = Lens.lens (\ConfigIdResponse' {configType} -> configType) (\s@ConfigIdResponse' {} a -> s {configType = a} :: ConfigIdResponse)

instance Data.FromJSON ConfigIdResponse where
  parseJSON =
    Data.withObject
      "ConfigIdResponse"
      ( \x ->
          ConfigIdResponse'
            Prelude.<$> (x Data..:? "configArn")
            Prelude.<*> (x Data..:? "configId")
            Prelude.<*> (x Data..:? "configType")
      )

instance Prelude.Hashable ConfigIdResponse where
  hashWithSalt _salt ConfigIdResponse' {..} =
    _salt `Prelude.hashWithSalt` configArn
      `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType

instance Prelude.NFData ConfigIdResponse where
  rnf ConfigIdResponse' {..} =
    Prelude.rnf configArn
      `Prelude.seq` Prelude.rnf configId
      `Prelude.seq` Prelude.rnf configType
