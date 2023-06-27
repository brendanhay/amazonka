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
-- Module      : Amazonka.TNB.Types.ListSolNetworkPackageInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolNetworkPackageInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.ListSolNetworkPackageMetadata
import Amazonka.TNB.Types.NsdOnboardingState
import Amazonka.TNB.Types.NsdOperationalState
import Amazonka.TNB.Types.NsdUsageState

-- | Details of a network package.
--
-- A network package is a .zip file in CSAR (Cloud Service Archive) format
-- defines the function packages you want to deploy and the Amazon Web
-- Services infrastructure you want to deploy them on.
--
-- /See:/ 'newListSolNetworkPackageInfo' smart constructor.
data ListSolNetworkPackageInfo = ListSolNetworkPackageInfo'
  { -- | Designer of the onboarded network service descriptor in the network
    -- package.
    nsdDesigner :: Prelude.Maybe Prelude.Text,
    -- | ID of the network service descriptor on which the network package is
    -- based.
    nsdId :: Prelude.Maybe Prelude.Text,
    -- | Identifies a network service descriptor in a version independent manner.
    nsdInvariantId :: Prelude.Maybe Prelude.Text,
    -- | Name of the onboarded network service descriptor in the network package.
    nsdName :: Prelude.Maybe Prelude.Text,
    -- | Version of the onboarded network service descriptor in the network
    -- package.
    nsdVersion :: Prelude.Maybe Prelude.Text,
    -- | Identifies the function package for the function package descriptor
    -- referenced by the onboarded network package.
    vnfPkgIds :: Prelude.Maybe [Prelude.Text],
    -- | Network package ARN.
    arn :: Prelude.Text,
    -- | ID of the individual network package.
    id :: Prelude.Text,
    -- | The metadata of the network package.
    metadata :: ListSolNetworkPackageMetadata,
    -- | Onboarding state of the network service descriptor in the network
    -- package.
    nsdOnboardingState :: NsdOnboardingState,
    -- | Operational state of the network service descriptor in the network
    -- package.
    nsdOperationalState :: NsdOperationalState,
    -- | Usage state of the network service descriptor in the network package.
    nsdUsageState :: NsdUsageState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkPackageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsdDesigner', 'listSolNetworkPackageInfo_nsdDesigner' - Designer of the onboarded network service descriptor in the network
-- package.
--
-- 'nsdId', 'listSolNetworkPackageInfo_nsdId' - ID of the network service descriptor on which the network package is
-- based.
--
-- 'nsdInvariantId', 'listSolNetworkPackageInfo_nsdInvariantId' - Identifies a network service descriptor in a version independent manner.
--
-- 'nsdName', 'listSolNetworkPackageInfo_nsdName' - Name of the onboarded network service descriptor in the network package.
--
-- 'nsdVersion', 'listSolNetworkPackageInfo_nsdVersion' - Version of the onboarded network service descriptor in the network
-- package.
--
-- 'vnfPkgIds', 'listSolNetworkPackageInfo_vnfPkgIds' - Identifies the function package for the function package descriptor
-- referenced by the onboarded network package.
--
-- 'arn', 'listSolNetworkPackageInfo_arn' - Network package ARN.
--
-- 'id', 'listSolNetworkPackageInfo_id' - ID of the individual network package.
--
-- 'metadata', 'listSolNetworkPackageInfo_metadata' - The metadata of the network package.
--
-- 'nsdOnboardingState', 'listSolNetworkPackageInfo_nsdOnboardingState' - Onboarding state of the network service descriptor in the network
-- package.
--
-- 'nsdOperationalState', 'listSolNetworkPackageInfo_nsdOperationalState' - Operational state of the network service descriptor in the network
-- package.
--
-- 'nsdUsageState', 'listSolNetworkPackageInfo_nsdUsageState' - Usage state of the network service descriptor in the network package.
newListSolNetworkPackageInfo ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  ListSolNetworkPackageMetadata ->
  -- | 'nsdOnboardingState'
  NsdOnboardingState ->
  -- | 'nsdOperationalState'
  NsdOperationalState ->
  -- | 'nsdUsageState'
  NsdUsageState ->
  ListSolNetworkPackageInfo
newListSolNetworkPackageInfo
  pArn_
  pId_
  pMetadata_
  pNsdOnboardingState_
  pNsdOperationalState_
  pNsdUsageState_ =
    ListSolNetworkPackageInfo'
      { nsdDesigner =
          Prelude.Nothing,
        nsdId = Prelude.Nothing,
        nsdInvariantId = Prelude.Nothing,
        nsdName = Prelude.Nothing,
        nsdVersion = Prelude.Nothing,
        vnfPkgIds = Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        metadata = pMetadata_,
        nsdOnboardingState = pNsdOnboardingState_,
        nsdOperationalState = pNsdOperationalState_,
        nsdUsageState = pNsdUsageState_
      }

-- | Designer of the onboarded network service descriptor in the network
-- package.
listSolNetworkPackageInfo_nsdDesigner :: Lens.Lens' ListSolNetworkPackageInfo (Prelude.Maybe Prelude.Text)
listSolNetworkPackageInfo_nsdDesigner = Lens.lens (\ListSolNetworkPackageInfo' {nsdDesigner} -> nsdDesigner) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdDesigner = a} :: ListSolNetworkPackageInfo)

-- | ID of the network service descriptor on which the network package is
-- based.
listSolNetworkPackageInfo_nsdId :: Lens.Lens' ListSolNetworkPackageInfo (Prelude.Maybe Prelude.Text)
listSolNetworkPackageInfo_nsdId = Lens.lens (\ListSolNetworkPackageInfo' {nsdId} -> nsdId) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdId = a} :: ListSolNetworkPackageInfo)

-- | Identifies a network service descriptor in a version independent manner.
listSolNetworkPackageInfo_nsdInvariantId :: Lens.Lens' ListSolNetworkPackageInfo (Prelude.Maybe Prelude.Text)
listSolNetworkPackageInfo_nsdInvariantId = Lens.lens (\ListSolNetworkPackageInfo' {nsdInvariantId} -> nsdInvariantId) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdInvariantId = a} :: ListSolNetworkPackageInfo)

-- | Name of the onboarded network service descriptor in the network package.
listSolNetworkPackageInfo_nsdName :: Lens.Lens' ListSolNetworkPackageInfo (Prelude.Maybe Prelude.Text)
listSolNetworkPackageInfo_nsdName = Lens.lens (\ListSolNetworkPackageInfo' {nsdName} -> nsdName) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdName = a} :: ListSolNetworkPackageInfo)

-- | Version of the onboarded network service descriptor in the network
-- package.
listSolNetworkPackageInfo_nsdVersion :: Lens.Lens' ListSolNetworkPackageInfo (Prelude.Maybe Prelude.Text)
listSolNetworkPackageInfo_nsdVersion = Lens.lens (\ListSolNetworkPackageInfo' {nsdVersion} -> nsdVersion) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdVersion = a} :: ListSolNetworkPackageInfo)

-- | Identifies the function package for the function package descriptor
-- referenced by the onboarded network package.
listSolNetworkPackageInfo_vnfPkgIds :: Lens.Lens' ListSolNetworkPackageInfo (Prelude.Maybe [Prelude.Text])
listSolNetworkPackageInfo_vnfPkgIds = Lens.lens (\ListSolNetworkPackageInfo' {vnfPkgIds} -> vnfPkgIds) (\s@ListSolNetworkPackageInfo' {} a -> s {vnfPkgIds = a} :: ListSolNetworkPackageInfo) Prelude.. Lens.mapping Lens.coerced

-- | Network package ARN.
listSolNetworkPackageInfo_arn :: Lens.Lens' ListSolNetworkPackageInfo Prelude.Text
listSolNetworkPackageInfo_arn = Lens.lens (\ListSolNetworkPackageInfo' {arn} -> arn) (\s@ListSolNetworkPackageInfo' {} a -> s {arn = a} :: ListSolNetworkPackageInfo)

-- | ID of the individual network package.
listSolNetworkPackageInfo_id :: Lens.Lens' ListSolNetworkPackageInfo Prelude.Text
listSolNetworkPackageInfo_id = Lens.lens (\ListSolNetworkPackageInfo' {id} -> id) (\s@ListSolNetworkPackageInfo' {} a -> s {id = a} :: ListSolNetworkPackageInfo)

-- | The metadata of the network package.
listSolNetworkPackageInfo_metadata :: Lens.Lens' ListSolNetworkPackageInfo ListSolNetworkPackageMetadata
listSolNetworkPackageInfo_metadata = Lens.lens (\ListSolNetworkPackageInfo' {metadata} -> metadata) (\s@ListSolNetworkPackageInfo' {} a -> s {metadata = a} :: ListSolNetworkPackageInfo)

-- | Onboarding state of the network service descriptor in the network
-- package.
listSolNetworkPackageInfo_nsdOnboardingState :: Lens.Lens' ListSolNetworkPackageInfo NsdOnboardingState
listSolNetworkPackageInfo_nsdOnboardingState = Lens.lens (\ListSolNetworkPackageInfo' {nsdOnboardingState} -> nsdOnboardingState) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdOnboardingState = a} :: ListSolNetworkPackageInfo)

-- | Operational state of the network service descriptor in the network
-- package.
listSolNetworkPackageInfo_nsdOperationalState :: Lens.Lens' ListSolNetworkPackageInfo NsdOperationalState
listSolNetworkPackageInfo_nsdOperationalState = Lens.lens (\ListSolNetworkPackageInfo' {nsdOperationalState} -> nsdOperationalState) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdOperationalState = a} :: ListSolNetworkPackageInfo)

-- | Usage state of the network service descriptor in the network package.
listSolNetworkPackageInfo_nsdUsageState :: Lens.Lens' ListSolNetworkPackageInfo NsdUsageState
listSolNetworkPackageInfo_nsdUsageState = Lens.lens (\ListSolNetworkPackageInfo' {nsdUsageState} -> nsdUsageState) (\s@ListSolNetworkPackageInfo' {} a -> s {nsdUsageState = a} :: ListSolNetworkPackageInfo)

instance Data.FromJSON ListSolNetworkPackageInfo where
  parseJSON =
    Data.withObject
      "ListSolNetworkPackageInfo"
      ( \x ->
          ListSolNetworkPackageInfo'
            Prelude.<$> (x Data..:? "nsdDesigner")
            Prelude.<*> (x Data..:? "nsdId")
            Prelude.<*> (x Data..:? "nsdInvariantId")
            Prelude.<*> (x Data..:? "nsdName")
            Prelude.<*> (x Data..:? "nsdVersion")
            Prelude.<*> (x Data..:? "vnfPkgIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "metadata")
            Prelude.<*> (x Data..: "nsdOnboardingState")
            Prelude.<*> (x Data..: "nsdOperationalState")
            Prelude.<*> (x Data..: "nsdUsageState")
      )

instance Prelude.Hashable ListSolNetworkPackageInfo where
  hashWithSalt _salt ListSolNetworkPackageInfo' {..} =
    _salt
      `Prelude.hashWithSalt` nsdDesigner
      `Prelude.hashWithSalt` nsdId
      `Prelude.hashWithSalt` nsdInvariantId
      `Prelude.hashWithSalt` nsdName
      `Prelude.hashWithSalt` nsdVersion
      `Prelude.hashWithSalt` vnfPkgIds
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` nsdOnboardingState
      `Prelude.hashWithSalt` nsdOperationalState
      `Prelude.hashWithSalt` nsdUsageState

instance Prelude.NFData ListSolNetworkPackageInfo where
  rnf ListSolNetworkPackageInfo' {..} =
    Prelude.rnf nsdDesigner
      `Prelude.seq` Prelude.rnf nsdId
      `Prelude.seq` Prelude.rnf nsdInvariantId
      `Prelude.seq` Prelude.rnf nsdName
      `Prelude.seq` Prelude.rnf nsdVersion
      `Prelude.seq` Prelude.rnf vnfPkgIds
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsdOnboardingState
      `Prelude.seq` Prelude.rnf nsdOperationalState
      `Prelude.seq` Prelude.rnf nsdUsageState
