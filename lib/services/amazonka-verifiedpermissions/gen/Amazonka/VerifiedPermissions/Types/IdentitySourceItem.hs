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
-- Module      : Amazonka.VerifiedPermissions.Types.IdentitySourceItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.IdentitySourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.IdentitySourceItemDetails

-- | A structure that defines an identity source.
--
-- This data type is used as a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListIdentityStores.html ListIdentityStores>
-- operation.
--
-- /See:/ 'newIdentitySourceItem' smart constructor.
data IdentitySourceItem = IdentitySourceItem'
  { -- | The date and time the identity source was originally created.
    createdDate :: Data.ISO8601,
    -- | A structure that contains the details of the associated identity
    -- provider (IdP).
    details :: IdentitySourceItemDetails,
    -- | The unique identifier of the identity source.
    identitySourceId :: Prelude.Text,
    -- | The date and time the identity source was most recently updated.
    lastUpdatedDate :: Data.ISO8601,
    -- | The identifier of the policy store that contains the identity source.
    policyStoreId :: Prelude.Text,
    -- | The Cedar entity type of the principals returned from the IdP associated
    -- with this identity source.
    principalEntityType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentitySourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'identitySourceItem_createdDate' - The date and time the identity source was originally created.
--
-- 'details', 'identitySourceItem_details' - A structure that contains the details of the associated identity
-- provider (IdP).
--
-- 'identitySourceId', 'identitySourceItem_identitySourceId' - The unique identifier of the identity source.
--
-- 'lastUpdatedDate', 'identitySourceItem_lastUpdatedDate' - The date and time the identity source was most recently updated.
--
-- 'policyStoreId', 'identitySourceItem_policyStoreId' - The identifier of the policy store that contains the identity source.
--
-- 'principalEntityType', 'identitySourceItem_principalEntityType' - The Cedar entity type of the principals returned from the IdP associated
-- with this identity source.
newIdentitySourceItem ::
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'details'
  IdentitySourceItemDetails ->
  -- | 'identitySourceId'
  Prelude.Text ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'principalEntityType'
  Prelude.Text ->
  IdentitySourceItem
newIdentitySourceItem
  pCreatedDate_
  pDetails_
  pIdentitySourceId_
  pLastUpdatedDate_
  pPolicyStoreId_
  pPrincipalEntityType_ =
    IdentitySourceItem'
      { createdDate =
          Data._Time Lens.# pCreatedDate_,
        details = pDetails_,
        identitySourceId = pIdentitySourceId_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_,
        policyStoreId = pPolicyStoreId_,
        principalEntityType = pPrincipalEntityType_
      }

-- | The date and time the identity source was originally created.
identitySourceItem_createdDate :: Lens.Lens' IdentitySourceItem Prelude.UTCTime
identitySourceItem_createdDate = Lens.lens (\IdentitySourceItem' {createdDate} -> createdDate) (\s@IdentitySourceItem' {} a -> s {createdDate = a} :: IdentitySourceItem) Prelude.. Data._Time

-- | A structure that contains the details of the associated identity
-- provider (IdP).
identitySourceItem_details :: Lens.Lens' IdentitySourceItem IdentitySourceItemDetails
identitySourceItem_details = Lens.lens (\IdentitySourceItem' {details} -> details) (\s@IdentitySourceItem' {} a -> s {details = a} :: IdentitySourceItem)

-- | The unique identifier of the identity source.
identitySourceItem_identitySourceId :: Lens.Lens' IdentitySourceItem Prelude.Text
identitySourceItem_identitySourceId = Lens.lens (\IdentitySourceItem' {identitySourceId} -> identitySourceId) (\s@IdentitySourceItem' {} a -> s {identitySourceId = a} :: IdentitySourceItem)

-- | The date and time the identity source was most recently updated.
identitySourceItem_lastUpdatedDate :: Lens.Lens' IdentitySourceItem Prelude.UTCTime
identitySourceItem_lastUpdatedDate = Lens.lens (\IdentitySourceItem' {lastUpdatedDate} -> lastUpdatedDate) (\s@IdentitySourceItem' {} a -> s {lastUpdatedDate = a} :: IdentitySourceItem) Prelude.. Data._Time

-- | The identifier of the policy store that contains the identity source.
identitySourceItem_policyStoreId :: Lens.Lens' IdentitySourceItem Prelude.Text
identitySourceItem_policyStoreId = Lens.lens (\IdentitySourceItem' {policyStoreId} -> policyStoreId) (\s@IdentitySourceItem' {} a -> s {policyStoreId = a} :: IdentitySourceItem)

-- | The Cedar entity type of the principals returned from the IdP associated
-- with this identity source.
identitySourceItem_principalEntityType :: Lens.Lens' IdentitySourceItem Prelude.Text
identitySourceItem_principalEntityType = Lens.lens (\IdentitySourceItem' {principalEntityType} -> principalEntityType) (\s@IdentitySourceItem' {} a -> s {principalEntityType = a} :: IdentitySourceItem)

instance Data.FromJSON IdentitySourceItem where
  parseJSON =
    Data.withObject
      "IdentitySourceItem"
      ( \x ->
          IdentitySourceItem'
            Prelude.<$> (x Data..: "createdDate")
            Prelude.<*> (x Data..: "details")
            Prelude.<*> (x Data..: "identitySourceId")
            Prelude.<*> (x Data..: "lastUpdatedDate")
            Prelude.<*> (x Data..: "policyStoreId")
            Prelude.<*> (x Data..: "principalEntityType")
      )

instance Prelude.Hashable IdentitySourceItem where
  hashWithSalt _salt IdentitySourceItem' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` identitySourceId
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` principalEntityType

instance Prelude.NFData IdentitySourceItem where
  rnf IdentitySourceItem' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf identitySourceId
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf principalEntityType
