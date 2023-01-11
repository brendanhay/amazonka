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
-- Module      : Amazonka.DataExchange.Types.LFTagPolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.LFTagPolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.LFResourceDetails
import Amazonka.DataExchange.Types.LFResourceType
import qualified Amazonka.Prelude as Prelude

-- | Details about the LF-tag policy.
--
-- /See:/ 'newLFTagPolicyDetails' smart constructor.
data LFTagPolicyDetails = LFTagPolicyDetails'
  { -- | The identifier for the AWS Glue Data Catalog.
    catalogId :: Prelude.Text,
    -- | The resource type for which the LF-tag policy applies.
    resourceType :: LFResourceType,
    -- | Details for the Lake Formation Resources included in the LF-tag policy.
    resourceDetails :: LFResourceDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFTagPolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'lFTagPolicyDetails_catalogId' - The identifier for the AWS Glue Data Catalog.
--
-- 'resourceType', 'lFTagPolicyDetails_resourceType' - The resource type for which the LF-tag policy applies.
--
-- 'resourceDetails', 'lFTagPolicyDetails_resourceDetails' - Details for the Lake Formation Resources included in the LF-tag policy.
newLFTagPolicyDetails ::
  -- | 'catalogId'
  Prelude.Text ->
  -- | 'resourceType'
  LFResourceType ->
  -- | 'resourceDetails'
  LFResourceDetails ->
  LFTagPolicyDetails
newLFTagPolicyDetails
  pCatalogId_
  pResourceType_
  pResourceDetails_ =
    LFTagPolicyDetails'
      { catalogId = pCatalogId_,
        resourceType = pResourceType_,
        resourceDetails = pResourceDetails_
      }

-- | The identifier for the AWS Glue Data Catalog.
lFTagPolicyDetails_catalogId :: Lens.Lens' LFTagPolicyDetails Prelude.Text
lFTagPolicyDetails_catalogId = Lens.lens (\LFTagPolicyDetails' {catalogId} -> catalogId) (\s@LFTagPolicyDetails' {} a -> s {catalogId = a} :: LFTagPolicyDetails)

-- | The resource type for which the LF-tag policy applies.
lFTagPolicyDetails_resourceType :: Lens.Lens' LFTagPolicyDetails LFResourceType
lFTagPolicyDetails_resourceType = Lens.lens (\LFTagPolicyDetails' {resourceType} -> resourceType) (\s@LFTagPolicyDetails' {} a -> s {resourceType = a} :: LFTagPolicyDetails)

-- | Details for the Lake Formation Resources included in the LF-tag policy.
lFTagPolicyDetails_resourceDetails :: Lens.Lens' LFTagPolicyDetails LFResourceDetails
lFTagPolicyDetails_resourceDetails = Lens.lens (\LFTagPolicyDetails' {resourceDetails} -> resourceDetails) (\s@LFTagPolicyDetails' {} a -> s {resourceDetails = a} :: LFTagPolicyDetails)

instance Data.FromJSON LFTagPolicyDetails where
  parseJSON =
    Data.withObject
      "LFTagPolicyDetails"
      ( \x ->
          LFTagPolicyDetails'
            Prelude.<$> (x Data..: "CatalogId")
            Prelude.<*> (x Data..: "ResourceType")
            Prelude.<*> (x Data..: "ResourceDetails")
      )

instance Prelude.Hashable LFTagPolicyDetails where
  hashWithSalt _salt LFTagPolicyDetails' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceDetails

instance Prelude.NFData LFTagPolicyDetails where
  rnf LFTagPolicyDetails' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceDetails
