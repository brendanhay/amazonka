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
-- Module      : Amazonka.IoTAnalytics.Types.IotEventsDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.IotEventsDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for delivery of dataset contents to IoT
-- Events.
--
-- /See:/ 'newIotEventsDestinationConfiguration' smart constructor.
data IotEventsDestinationConfiguration = IotEventsDestinationConfiguration'
  { -- | The name of the IoT Events input to which dataset contents are
    -- delivered.
    inputName :: Prelude.Text,
    -- | The ARN of the role that grants IoT Analytics permission to deliver
    -- dataset contents to an IoT Events input.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotEventsDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputName', 'iotEventsDestinationConfiguration_inputName' - The name of the IoT Events input to which dataset contents are
-- delivered.
--
-- 'roleArn', 'iotEventsDestinationConfiguration_roleArn' - The ARN of the role that grants IoT Analytics permission to deliver
-- dataset contents to an IoT Events input.
newIotEventsDestinationConfiguration ::
  -- | 'inputName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  IotEventsDestinationConfiguration
newIotEventsDestinationConfiguration
  pInputName_
  pRoleArn_ =
    IotEventsDestinationConfiguration'
      { inputName =
          pInputName_,
        roleArn = pRoleArn_
      }

-- | The name of the IoT Events input to which dataset contents are
-- delivered.
iotEventsDestinationConfiguration_inputName :: Lens.Lens' IotEventsDestinationConfiguration Prelude.Text
iotEventsDestinationConfiguration_inputName = Lens.lens (\IotEventsDestinationConfiguration' {inputName} -> inputName) (\s@IotEventsDestinationConfiguration' {} a -> s {inputName = a} :: IotEventsDestinationConfiguration)

-- | The ARN of the role that grants IoT Analytics permission to deliver
-- dataset contents to an IoT Events input.
iotEventsDestinationConfiguration_roleArn :: Lens.Lens' IotEventsDestinationConfiguration Prelude.Text
iotEventsDestinationConfiguration_roleArn = Lens.lens (\IotEventsDestinationConfiguration' {roleArn} -> roleArn) (\s@IotEventsDestinationConfiguration' {} a -> s {roleArn = a} :: IotEventsDestinationConfiguration)

instance
  Data.FromJSON
    IotEventsDestinationConfiguration
  where
  parseJSON =
    Data.withObject
      "IotEventsDestinationConfiguration"
      ( \x ->
          IotEventsDestinationConfiguration'
            Prelude.<$> (x Data..: "inputName")
            Prelude.<*> (x Data..: "roleArn")
      )

instance
  Prelude.Hashable
    IotEventsDestinationConfiguration
  where
  hashWithSalt
    _salt
    IotEventsDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` inputName
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    IotEventsDestinationConfiguration
  where
  rnf IotEventsDestinationConfiguration' {..} =
    Prelude.rnf inputName
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToJSON
    IotEventsDestinationConfiguration
  where
  toJSON IotEventsDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("inputName" Data..= inputName),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
