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
-- Module      : Network.AWS.SageMaker.Types.MonitoringInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.EndpointInput

-- | The inputs for a monitoring job.
--
-- /See:/ 'newMonitoringInput' smart constructor.
data MonitoringInput = MonitoringInput'
  { -- | The endpoint for a monitoring job.
    endpointInput :: EndpointInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MonitoringInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointInput', 'monitoringInput_endpointInput' - The endpoint for a monitoring job.
newMonitoringInput ::
  -- | 'endpointInput'
  EndpointInput ->
  MonitoringInput
newMonitoringInput pEndpointInput_ =
  MonitoringInput' {endpointInput = pEndpointInput_}

-- | The endpoint for a monitoring job.
monitoringInput_endpointInput :: Lens.Lens' MonitoringInput EndpointInput
monitoringInput_endpointInput = Lens.lens (\MonitoringInput' {endpointInput} -> endpointInput) (\s@MonitoringInput' {} a -> s {endpointInput = a} :: MonitoringInput)

instance Prelude.FromJSON MonitoringInput where
  parseJSON =
    Prelude.withObject
      "MonitoringInput"
      ( \x ->
          MonitoringInput'
            Prelude.<$> (x Prelude..: "EndpointInput")
      )

instance Prelude.Hashable MonitoringInput

instance Prelude.NFData MonitoringInput

instance Prelude.ToJSON MonitoringInput where
  toJSON MonitoringInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointInput" Prelude..= endpointInput)
          ]
      )
