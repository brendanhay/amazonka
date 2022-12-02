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
-- Module      : Amazonka.DataBrew.Types.EntityDetectorConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.EntityDetectorConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.AllowedStatistics
import qualified Amazonka.Prelude as Prelude

-- | Configuration of entity detection for a profile job. When undefined,
-- entity detection is disabled.
--
-- /See:/ 'newEntityDetectorConfiguration' smart constructor.
data EntityDetectorConfiguration = EntityDetectorConfiguration'
  { -- | Configuration of statistics that are allowed to be run on columns that
    -- contain detected entities. When undefined, no statistics will be
    -- computed on columns that contain detected entities.
    allowedStatistics :: Prelude.Maybe (Prelude.NonEmpty AllowedStatistics),
    -- | Entity types to detect. Can be any of the following:
    --
    -- -   USA_SSN
    --
    -- -   EMAIL
    --
    -- -   USA_ITIN
    --
    -- -   USA_PASSPORT_NUMBER
    --
    -- -   PHONE_NUMBER
    --
    -- -   USA_DRIVING_LICENSE
    --
    -- -   BANK_ACCOUNT
    --
    -- -   CREDIT_CARD
    --
    -- -   IP_ADDRESS
    --
    -- -   MAC_ADDRESS
    --
    -- -   USA_DEA_NUMBER
    --
    -- -   USA_HCPCS_CODE
    --
    -- -   USA_NATIONAL_PROVIDER_IDENTIFIER
    --
    -- -   USA_NATIONAL_DRUG_CODE
    --
    -- -   USA_HEALTH_INSURANCE_CLAIM_NUMBER
    --
    -- -   USA_MEDICARE_BENEFICIARY_IDENTIFIER
    --
    -- -   USA_CPT_CODE
    --
    -- -   PERSON_NAME
    --
    -- -   DATE
    --
    -- The Entity type group USA_ALL is also supported, and includes all of the
    -- above entity types except PERSON_NAME and DATE.
    entityTypes :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityDetectorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedStatistics', 'entityDetectorConfiguration_allowedStatistics' - Configuration of statistics that are allowed to be run on columns that
-- contain detected entities. When undefined, no statistics will be
-- computed on columns that contain detected entities.
--
-- 'entityTypes', 'entityDetectorConfiguration_entityTypes' - Entity types to detect. Can be any of the following:
--
-- -   USA_SSN
--
-- -   EMAIL
--
-- -   USA_ITIN
--
-- -   USA_PASSPORT_NUMBER
--
-- -   PHONE_NUMBER
--
-- -   USA_DRIVING_LICENSE
--
-- -   BANK_ACCOUNT
--
-- -   CREDIT_CARD
--
-- -   IP_ADDRESS
--
-- -   MAC_ADDRESS
--
-- -   USA_DEA_NUMBER
--
-- -   USA_HCPCS_CODE
--
-- -   USA_NATIONAL_PROVIDER_IDENTIFIER
--
-- -   USA_NATIONAL_DRUG_CODE
--
-- -   USA_HEALTH_INSURANCE_CLAIM_NUMBER
--
-- -   USA_MEDICARE_BENEFICIARY_IDENTIFIER
--
-- -   USA_CPT_CODE
--
-- -   PERSON_NAME
--
-- -   DATE
--
-- The Entity type group USA_ALL is also supported, and includes all of the
-- above entity types except PERSON_NAME and DATE.
newEntityDetectorConfiguration ::
  -- | 'entityTypes'
  Prelude.NonEmpty Prelude.Text ->
  EntityDetectorConfiguration
newEntityDetectorConfiguration pEntityTypes_ =
  EntityDetectorConfiguration'
    { allowedStatistics =
        Prelude.Nothing,
      entityTypes =
        Lens.coerced Lens.# pEntityTypes_
    }

-- | Configuration of statistics that are allowed to be run on columns that
-- contain detected entities. When undefined, no statistics will be
-- computed on columns that contain detected entities.
entityDetectorConfiguration_allowedStatistics :: Lens.Lens' EntityDetectorConfiguration (Prelude.Maybe (Prelude.NonEmpty AllowedStatistics))
entityDetectorConfiguration_allowedStatistics = Lens.lens (\EntityDetectorConfiguration' {allowedStatistics} -> allowedStatistics) (\s@EntityDetectorConfiguration' {} a -> s {allowedStatistics = a} :: EntityDetectorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Entity types to detect. Can be any of the following:
--
-- -   USA_SSN
--
-- -   EMAIL
--
-- -   USA_ITIN
--
-- -   USA_PASSPORT_NUMBER
--
-- -   PHONE_NUMBER
--
-- -   USA_DRIVING_LICENSE
--
-- -   BANK_ACCOUNT
--
-- -   CREDIT_CARD
--
-- -   IP_ADDRESS
--
-- -   MAC_ADDRESS
--
-- -   USA_DEA_NUMBER
--
-- -   USA_HCPCS_CODE
--
-- -   USA_NATIONAL_PROVIDER_IDENTIFIER
--
-- -   USA_NATIONAL_DRUG_CODE
--
-- -   USA_HEALTH_INSURANCE_CLAIM_NUMBER
--
-- -   USA_MEDICARE_BENEFICIARY_IDENTIFIER
--
-- -   USA_CPT_CODE
--
-- -   PERSON_NAME
--
-- -   DATE
--
-- The Entity type group USA_ALL is also supported, and includes all of the
-- above entity types except PERSON_NAME and DATE.
entityDetectorConfiguration_entityTypes :: Lens.Lens' EntityDetectorConfiguration (Prelude.NonEmpty Prelude.Text)
entityDetectorConfiguration_entityTypes = Lens.lens (\EntityDetectorConfiguration' {entityTypes} -> entityTypes) (\s@EntityDetectorConfiguration' {} a -> s {entityTypes = a} :: EntityDetectorConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON EntityDetectorConfiguration where
  parseJSON =
    Data.withObject
      "EntityDetectorConfiguration"
      ( \x ->
          EntityDetectorConfiguration'
            Prelude.<$> (x Data..:? "AllowedStatistics")
            Prelude.<*> (x Data..: "EntityTypes")
      )

instance Prelude.Hashable EntityDetectorConfiguration where
  hashWithSalt _salt EntityDetectorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` allowedStatistics
      `Prelude.hashWithSalt` entityTypes

instance Prelude.NFData EntityDetectorConfiguration where
  rnf EntityDetectorConfiguration' {..} =
    Prelude.rnf allowedStatistics
      `Prelude.seq` Prelude.rnf entityTypes

instance Data.ToJSON EntityDetectorConfiguration where
  toJSON EntityDetectorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowedStatistics" Data..=)
              Prelude.<$> allowedStatistics,
            Prelude.Just ("EntityTypes" Data..= entityTypes)
          ]
      )
