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
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointConfigSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary information for an endpoint configuration.
--
-- /See:/ 'newEndpointConfigSummary' smart constructor.
data EndpointConfigSummary = EndpointConfigSummary'
  { -- | The name of the endpoint configuration.
    endpointConfigName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigArn :: Prelude.Text,
    -- | A timestamp that shows when the endpoint configuration was created.
    creationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'endpointConfigArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  EndpointConfigSummary
newEndpointConfigSummary
  pEndpointConfigName_
  pEndpointConfigArn_
  pCreationTime_ =
    EndpointConfigSummary'
      { endpointConfigName =
          pEndpointConfigName_,
        endpointConfigArn = pEndpointConfigArn_,
        creationTime = Prelude._Time Lens.# pCreationTime_
      }

-- | The name of the endpoint configuration.
endpointConfigSummary_endpointConfigName :: Lens.Lens' EndpointConfigSummary Prelude.Text
endpointConfigSummary_endpointConfigName = Lens.lens (\EndpointConfigSummary' {endpointConfigName} -> endpointConfigName) (\s@EndpointConfigSummary' {} a -> s {endpointConfigName = a} :: EndpointConfigSummary)

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
endpointConfigSummary_endpointConfigArn :: Lens.Lens' EndpointConfigSummary Prelude.Text
endpointConfigSummary_endpointConfigArn = Lens.lens (\EndpointConfigSummary' {endpointConfigArn} -> endpointConfigArn) (\s@EndpointConfigSummary' {} a -> s {endpointConfigArn = a} :: EndpointConfigSummary)

-- | A timestamp that shows when the endpoint configuration was created.
endpointConfigSummary_creationTime :: Lens.Lens' EndpointConfigSummary Prelude.UTCTime
endpointConfigSummary_creationTime = Lens.lens (\EndpointConfigSummary' {creationTime} -> creationTime) (\s@EndpointConfigSummary' {} a -> s {creationTime = a} :: EndpointConfigSummary) Prelude.. Prelude._Time

instance Prelude.FromJSON EndpointConfigSummary where
  parseJSON =
    Prelude.withObject
      "EndpointConfigSummary"
      ( \x ->
          EndpointConfigSummary'
            Prelude.<$> (x Prelude..: "EndpointConfigName")
            Prelude.<*> (x Prelude..: "EndpointConfigArn")
            Prelude.<*> (x Prelude..: "CreationTime")
      )

instance Prelude.Hashable EndpointConfigSummary

instance Prelude.NFData EndpointConfigSummary
