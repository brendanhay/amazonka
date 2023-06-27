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
-- Module      : Amazonka.TNB.Types.ListSolFunctionPackageInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolFunctionPackageInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.ListSolFunctionPackageMetadata
import Amazonka.TNB.Types.OnboardingState
import Amazonka.TNB.Types.OperationalState
import Amazonka.TNB.Types.UsageState

-- | Information about a function package.
--
-- A function package is a .zip file in CSAR (Cloud Service Archive) format
-- that contains a network function (an ETSI standard telecommunication
-- application) and function package descriptor that uses the TOSCA
-- standard to describe how the network functions should run on your
-- network.
--
-- /See:/ 'newListSolFunctionPackageInfo' smart constructor.
data ListSolFunctionPackageInfo = ListSolFunctionPackageInfo'
  { -- | The metadata of the function package.
    metadata :: Prelude.Maybe ListSolFunctionPackageMetadata,
    -- | The product name for the network function.
    vnfProductName :: Prelude.Maybe Prelude.Text,
    -- | Provider of the function package and the function package descriptor.
    vnfProvider :: Prelude.Maybe Prelude.Text,
    -- | Identifies the function package and the function package descriptor.
    vnfdId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the version of the function package descriptor.
    vnfdVersion :: Prelude.Maybe Prelude.Text,
    -- | Function package ARN.
    arn :: Prelude.Text,
    -- | ID of the function package.
    id :: Prelude.Text,
    -- | Onboarding state of the function package.
    onboardingState :: OnboardingState,
    -- | Operational state of the function package.
    operationalState :: OperationalState,
    -- | Usage state of the function package.
    usageState :: UsageState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionPackageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'listSolFunctionPackageInfo_metadata' - The metadata of the function package.
--
-- 'vnfProductName', 'listSolFunctionPackageInfo_vnfProductName' - The product name for the network function.
--
-- 'vnfProvider', 'listSolFunctionPackageInfo_vnfProvider' - Provider of the function package and the function package descriptor.
--
-- 'vnfdId', 'listSolFunctionPackageInfo_vnfdId' - Identifies the function package and the function package descriptor.
--
-- 'vnfdVersion', 'listSolFunctionPackageInfo_vnfdVersion' - Identifies the version of the function package descriptor.
--
-- 'arn', 'listSolFunctionPackageInfo_arn' - Function package ARN.
--
-- 'id', 'listSolFunctionPackageInfo_id' - ID of the function package.
--
-- 'onboardingState', 'listSolFunctionPackageInfo_onboardingState' - Onboarding state of the function package.
--
-- 'operationalState', 'listSolFunctionPackageInfo_operationalState' - Operational state of the function package.
--
-- 'usageState', 'listSolFunctionPackageInfo_usageState' - Usage state of the function package.
newListSolFunctionPackageInfo ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'onboardingState'
  OnboardingState ->
  -- | 'operationalState'
  OperationalState ->
  -- | 'usageState'
  UsageState ->
  ListSolFunctionPackageInfo
newListSolFunctionPackageInfo
  pArn_
  pId_
  pOnboardingState_
  pOperationalState_
  pUsageState_ =
    ListSolFunctionPackageInfo'
      { metadata =
          Prelude.Nothing,
        vnfProductName = Prelude.Nothing,
        vnfProvider = Prelude.Nothing,
        vnfdId = Prelude.Nothing,
        vnfdVersion = Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        onboardingState = pOnboardingState_,
        operationalState = pOperationalState_,
        usageState = pUsageState_
      }

-- | The metadata of the function package.
listSolFunctionPackageInfo_metadata :: Lens.Lens' ListSolFunctionPackageInfo (Prelude.Maybe ListSolFunctionPackageMetadata)
listSolFunctionPackageInfo_metadata = Lens.lens (\ListSolFunctionPackageInfo' {metadata} -> metadata) (\s@ListSolFunctionPackageInfo' {} a -> s {metadata = a} :: ListSolFunctionPackageInfo)

-- | The product name for the network function.
listSolFunctionPackageInfo_vnfProductName :: Lens.Lens' ListSolFunctionPackageInfo (Prelude.Maybe Prelude.Text)
listSolFunctionPackageInfo_vnfProductName = Lens.lens (\ListSolFunctionPackageInfo' {vnfProductName} -> vnfProductName) (\s@ListSolFunctionPackageInfo' {} a -> s {vnfProductName = a} :: ListSolFunctionPackageInfo)

-- | Provider of the function package and the function package descriptor.
listSolFunctionPackageInfo_vnfProvider :: Lens.Lens' ListSolFunctionPackageInfo (Prelude.Maybe Prelude.Text)
listSolFunctionPackageInfo_vnfProvider = Lens.lens (\ListSolFunctionPackageInfo' {vnfProvider} -> vnfProvider) (\s@ListSolFunctionPackageInfo' {} a -> s {vnfProvider = a} :: ListSolFunctionPackageInfo)

-- | Identifies the function package and the function package descriptor.
listSolFunctionPackageInfo_vnfdId :: Lens.Lens' ListSolFunctionPackageInfo (Prelude.Maybe Prelude.Text)
listSolFunctionPackageInfo_vnfdId = Lens.lens (\ListSolFunctionPackageInfo' {vnfdId} -> vnfdId) (\s@ListSolFunctionPackageInfo' {} a -> s {vnfdId = a} :: ListSolFunctionPackageInfo)

-- | Identifies the version of the function package descriptor.
listSolFunctionPackageInfo_vnfdVersion :: Lens.Lens' ListSolFunctionPackageInfo (Prelude.Maybe Prelude.Text)
listSolFunctionPackageInfo_vnfdVersion = Lens.lens (\ListSolFunctionPackageInfo' {vnfdVersion} -> vnfdVersion) (\s@ListSolFunctionPackageInfo' {} a -> s {vnfdVersion = a} :: ListSolFunctionPackageInfo)

-- | Function package ARN.
listSolFunctionPackageInfo_arn :: Lens.Lens' ListSolFunctionPackageInfo Prelude.Text
listSolFunctionPackageInfo_arn = Lens.lens (\ListSolFunctionPackageInfo' {arn} -> arn) (\s@ListSolFunctionPackageInfo' {} a -> s {arn = a} :: ListSolFunctionPackageInfo)

-- | ID of the function package.
listSolFunctionPackageInfo_id :: Lens.Lens' ListSolFunctionPackageInfo Prelude.Text
listSolFunctionPackageInfo_id = Lens.lens (\ListSolFunctionPackageInfo' {id} -> id) (\s@ListSolFunctionPackageInfo' {} a -> s {id = a} :: ListSolFunctionPackageInfo)

-- | Onboarding state of the function package.
listSolFunctionPackageInfo_onboardingState :: Lens.Lens' ListSolFunctionPackageInfo OnboardingState
listSolFunctionPackageInfo_onboardingState = Lens.lens (\ListSolFunctionPackageInfo' {onboardingState} -> onboardingState) (\s@ListSolFunctionPackageInfo' {} a -> s {onboardingState = a} :: ListSolFunctionPackageInfo)

-- | Operational state of the function package.
listSolFunctionPackageInfo_operationalState :: Lens.Lens' ListSolFunctionPackageInfo OperationalState
listSolFunctionPackageInfo_operationalState = Lens.lens (\ListSolFunctionPackageInfo' {operationalState} -> operationalState) (\s@ListSolFunctionPackageInfo' {} a -> s {operationalState = a} :: ListSolFunctionPackageInfo)

-- | Usage state of the function package.
listSolFunctionPackageInfo_usageState :: Lens.Lens' ListSolFunctionPackageInfo UsageState
listSolFunctionPackageInfo_usageState = Lens.lens (\ListSolFunctionPackageInfo' {usageState} -> usageState) (\s@ListSolFunctionPackageInfo' {} a -> s {usageState = a} :: ListSolFunctionPackageInfo)

instance Data.FromJSON ListSolFunctionPackageInfo where
  parseJSON =
    Data.withObject
      "ListSolFunctionPackageInfo"
      ( \x ->
          ListSolFunctionPackageInfo'
            Prelude.<$> (x Data..:? "metadata")
            Prelude.<*> (x Data..:? "vnfProductName")
            Prelude.<*> (x Data..:? "vnfProvider")
            Prelude.<*> (x Data..:? "vnfdId")
            Prelude.<*> (x Data..:? "vnfdVersion")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "onboardingState")
            Prelude.<*> (x Data..: "operationalState")
            Prelude.<*> (x Data..: "usageState")
      )

instance Prelude.Hashable ListSolFunctionPackageInfo where
  hashWithSalt _salt ListSolFunctionPackageInfo' {..} =
    _salt
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` vnfProductName
      `Prelude.hashWithSalt` vnfProvider
      `Prelude.hashWithSalt` vnfdId
      `Prelude.hashWithSalt` vnfdVersion
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` onboardingState
      `Prelude.hashWithSalt` operationalState
      `Prelude.hashWithSalt` usageState

instance Prelude.NFData ListSolFunctionPackageInfo where
  rnf ListSolFunctionPackageInfo' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf vnfProductName
      `Prelude.seq` Prelude.rnf vnfProvider
      `Prelude.seq` Prelude.rnf vnfdId
      `Prelude.seq` Prelude.rnf vnfdVersion
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf onboardingState
      `Prelude.seq` Prelude.rnf operationalState
      `Prelude.seq` Prelude.rnf usageState
