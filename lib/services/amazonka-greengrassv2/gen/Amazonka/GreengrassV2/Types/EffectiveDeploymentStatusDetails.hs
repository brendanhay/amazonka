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
-- Module      : Amazonka.GreengrassV2.Types.EffectiveDeploymentStatusDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.EffectiveDeploymentStatusDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains all error-related information for the deployment record. The
-- status details will be null if the deployment is in a success state.
--
-- Greengrass nucleus v2.8.0 or later is required to get an accurate
-- @errorStack@ and @errorTypes@ response. This field will not be returned
-- for earlier Greengrass nucleus versions.
--
-- /See:/ 'newEffectiveDeploymentStatusDetails' smart constructor.
data EffectiveDeploymentStatusDetails = EffectiveDeploymentStatusDetails'
  { -- | Contains an ordered list of short error codes that range from the most
    -- generic error to the most specific one. The error codes describe the
    -- reason for failure whenever the @coreDeviceExecutionStatus@ is in a
    -- failed state. The response will be an empty list if there is no error.
    errorStack :: Prelude.Maybe [Prelude.Text],
    -- | Contains tags which describe the error. You can use the error types to
    -- classify errors to assist with remediating the failure. The response
    -- will be an empty list if there is no error.
    errorTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EffectiveDeploymentStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorStack', 'effectiveDeploymentStatusDetails_errorStack' - Contains an ordered list of short error codes that range from the most
-- generic error to the most specific one. The error codes describe the
-- reason for failure whenever the @coreDeviceExecutionStatus@ is in a
-- failed state. The response will be an empty list if there is no error.
--
-- 'errorTypes', 'effectiveDeploymentStatusDetails_errorTypes' - Contains tags which describe the error. You can use the error types to
-- classify errors to assist with remediating the failure. The response
-- will be an empty list if there is no error.
newEffectiveDeploymentStatusDetails ::
  EffectiveDeploymentStatusDetails
newEffectiveDeploymentStatusDetails =
  EffectiveDeploymentStatusDetails'
    { errorStack =
        Prelude.Nothing,
      errorTypes = Prelude.Nothing
    }

-- | Contains an ordered list of short error codes that range from the most
-- generic error to the most specific one. The error codes describe the
-- reason for failure whenever the @coreDeviceExecutionStatus@ is in a
-- failed state. The response will be an empty list if there is no error.
effectiveDeploymentStatusDetails_errorStack :: Lens.Lens' EffectiveDeploymentStatusDetails (Prelude.Maybe [Prelude.Text])
effectiveDeploymentStatusDetails_errorStack = Lens.lens (\EffectiveDeploymentStatusDetails' {errorStack} -> errorStack) (\s@EffectiveDeploymentStatusDetails' {} a -> s {errorStack = a} :: EffectiveDeploymentStatusDetails) Prelude.. Lens.mapping Lens.coerced

-- | Contains tags which describe the error. You can use the error types to
-- classify errors to assist with remediating the failure. The response
-- will be an empty list if there is no error.
effectiveDeploymentStatusDetails_errorTypes :: Lens.Lens' EffectiveDeploymentStatusDetails (Prelude.Maybe [Prelude.Text])
effectiveDeploymentStatusDetails_errorTypes = Lens.lens (\EffectiveDeploymentStatusDetails' {errorTypes} -> errorTypes) (\s@EffectiveDeploymentStatusDetails' {} a -> s {errorTypes = a} :: EffectiveDeploymentStatusDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    EffectiveDeploymentStatusDetails
  where
  parseJSON =
    Data.withObject
      "EffectiveDeploymentStatusDetails"
      ( \x ->
          EffectiveDeploymentStatusDetails'
            Prelude.<$> (x Data..:? "errorStack" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "errorTypes" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    EffectiveDeploymentStatusDetails
  where
  hashWithSalt
    _salt
    EffectiveDeploymentStatusDetails' {..} =
      _salt
        `Prelude.hashWithSalt` errorStack
        `Prelude.hashWithSalt` errorTypes

instance
  Prelude.NFData
    EffectiveDeploymentStatusDetails
  where
  rnf EffectiveDeploymentStatusDetails' {..} =
    Prelude.rnf errorStack `Prelude.seq`
      Prelude.rnf errorTypes
