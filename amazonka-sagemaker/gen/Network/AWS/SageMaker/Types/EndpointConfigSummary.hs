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
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointConfigSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides summary information for an endpoint configuration.
--
-- /See:/ 'newEndpointConfigSummary' smart constructor.
data EndpointConfigSummary = EndpointConfigSummary'
  { -- | The name of the endpoint configuration.
    endpointConfigName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigArn :: Core.Text,
    -- | A timestamp that shows when the endpoint configuration was created.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EndpointConfigSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigName', 'endpointConfigSummary_endpointConfigName' - The name of the endpoint configuration.
--
-- 'endpointConfigArn', 'endpointConfigSummary_endpointConfigArn' - The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- 'creationTime', 'endpointConfigSummary_creationTime' - A timestamp that shows when the endpoint configuration was created.
newEndpointConfigSummary ::
  -- | 'endpointConfigName'
  Core.Text ->
  -- | 'endpointConfigArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  EndpointConfigSummary
newEndpointConfigSummary
  pEndpointConfigName_
  pEndpointConfigArn_
  pCreationTime_ =
    EndpointConfigSummary'
      { endpointConfigName =
          pEndpointConfigName_,
        endpointConfigArn = pEndpointConfigArn_,
        creationTime = Core._Time Lens.# pCreationTime_
      }

-- | The name of the endpoint configuration.
endpointConfigSummary_endpointConfigName :: Lens.Lens' EndpointConfigSummary Core.Text
endpointConfigSummary_endpointConfigName = Lens.lens (\EndpointConfigSummary' {endpointConfigName} -> endpointConfigName) (\s@EndpointConfigSummary' {} a -> s {endpointConfigName = a} :: EndpointConfigSummary)

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
endpointConfigSummary_endpointConfigArn :: Lens.Lens' EndpointConfigSummary Core.Text
endpointConfigSummary_endpointConfigArn = Lens.lens (\EndpointConfigSummary' {endpointConfigArn} -> endpointConfigArn) (\s@EndpointConfigSummary' {} a -> s {endpointConfigArn = a} :: EndpointConfigSummary)

-- | A timestamp that shows when the endpoint configuration was created.
endpointConfigSummary_creationTime :: Lens.Lens' EndpointConfigSummary Core.UTCTime
endpointConfigSummary_creationTime = Lens.lens (\EndpointConfigSummary' {creationTime} -> creationTime) (\s@EndpointConfigSummary' {} a -> s {creationTime = a} :: EndpointConfigSummary) Core.. Core._Time

instance Core.FromJSON EndpointConfigSummary where
  parseJSON =
    Core.withObject
      "EndpointConfigSummary"
      ( \x ->
          EndpointConfigSummary'
            Core.<$> (x Core..: "EndpointConfigName")
            Core.<*> (x Core..: "EndpointConfigArn")
            Core.<*> (x Core..: "CreationTime")
      )

instance Core.Hashable EndpointConfigSummary

instance Core.NFData EndpointConfigSummary
