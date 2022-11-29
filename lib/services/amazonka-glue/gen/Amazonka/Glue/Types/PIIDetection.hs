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
-- Module      : Amazonka.Glue.Types.PIIDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PIIDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.PiiType
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that identifies, removes or masks PII data.
--
-- /See:/ 'newPIIDetection' smart constructor.
data PIIDetection = PIIDetection'
  { -- | Indicates the value that will replace the detected entity.
    maskValue :: Prelude.Maybe Prelude.Text,
    -- | Indicates the fraction of the data to sample when scanning for PII
    -- entities.
    sampleFraction :: Prelude.Maybe Prelude.Double,
    -- | Indicates the output column name that will contain any entity type
    -- detected in that row.
    outputColumnName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the fraction of the data that must be met in order for a
    -- column to be identified as PII data.
    thresholdFraction :: Prelude.Maybe Prelude.Double,
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The node ID inputs to the transform.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | Indicates the type of PIIDetection transform.
    piiType :: PiiType,
    -- | Indicates the types of entities the PIIDetection transform will identify
    -- as PII data.
    --
    -- PII type entities include: PERSON_NAME, DATE, USA_SNN, EMAIL, USA_ITIN,
    -- USA_PASSPORT_NUMBER, PHONE_NUMBER, BANK_ACCOUNT, IP_ADDRESS,
    -- MAC_ADDRESS, USA_CPT_CODE, USA_HCPCS_CODE, USA_NATIONAL_DRUG_CODE,
    -- USA_MEDICARE_BENEFICIARY_IDENTIFIER,
    -- USA_HEALTH_INSURANCE_CLAIM_NUMBER,CREDIT_CARD,USA_NATIONAL_PROVIDER_IDENTIFIER,USA_DEA_NUMBER,USA_DRIVING_LICENSE
    entityTypesToDetect :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PIIDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maskValue', 'pIIDetection_maskValue' - Indicates the value that will replace the detected entity.
--
-- 'sampleFraction', 'pIIDetection_sampleFraction' - Indicates the fraction of the data to sample when scanning for PII
-- entities.
--
-- 'outputColumnName', 'pIIDetection_outputColumnName' - Indicates the output column name that will contain any entity type
-- detected in that row.
--
-- 'thresholdFraction', 'pIIDetection_thresholdFraction' - Indicates the fraction of the data that must be met in order for a
-- column to be identified as PII data.
--
-- 'name', 'pIIDetection_name' - The name of the transform node.
--
-- 'inputs', 'pIIDetection_inputs' - The node ID inputs to the transform.
--
-- 'piiType', 'pIIDetection_piiType' - Indicates the type of PIIDetection transform.
--
-- 'entityTypesToDetect', 'pIIDetection_entityTypesToDetect' - Indicates the types of entities the PIIDetection transform will identify
-- as PII data.
--
-- PII type entities include: PERSON_NAME, DATE, USA_SNN, EMAIL, USA_ITIN,
-- USA_PASSPORT_NUMBER, PHONE_NUMBER, BANK_ACCOUNT, IP_ADDRESS,
-- MAC_ADDRESS, USA_CPT_CODE, USA_HCPCS_CODE, USA_NATIONAL_DRUG_CODE,
-- USA_MEDICARE_BENEFICIARY_IDENTIFIER,
-- USA_HEALTH_INSURANCE_CLAIM_NUMBER,CREDIT_CARD,USA_NATIONAL_PROVIDER_IDENTIFIER,USA_DEA_NUMBER,USA_DRIVING_LICENSE
newPIIDetection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'piiType'
  PiiType ->
  PIIDetection
newPIIDetection pName_ pInputs_ pPiiType_ =
  PIIDetection'
    { maskValue = Prelude.Nothing,
      sampleFraction = Prelude.Nothing,
      outputColumnName = Prelude.Nothing,
      thresholdFraction = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      piiType = pPiiType_,
      entityTypesToDetect = Prelude.mempty
    }

-- | Indicates the value that will replace the detected entity.
pIIDetection_maskValue :: Lens.Lens' PIIDetection (Prelude.Maybe Prelude.Text)
pIIDetection_maskValue = Lens.lens (\PIIDetection' {maskValue} -> maskValue) (\s@PIIDetection' {} a -> s {maskValue = a} :: PIIDetection)

-- | Indicates the fraction of the data to sample when scanning for PII
-- entities.
pIIDetection_sampleFraction :: Lens.Lens' PIIDetection (Prelude.Maybe Prelude.Double)
pIIDetection_sampleFraction = Lens.lens (\PIIDetection' {sampleFraction} -> sampleFraction) (\s@PIIDetection' {} a -> s {sampleFraction = a} :: PIIDetection)

-- | Indicates the output column name that will contain any entity type
-- detected in that row.
pIIDetection_outputColumnName :: Lens.Lens' PIIDetection (Prelude.Maybe Prelude.Text)
pIIDetection_outputColumnName = Lens.lens (\PIIDetection' {outputColumnName} -> outputColumnName) (\s@PIIDetection' {} a -> s {outputColumnName = a} :: PIIDetection)

-- | Indicates the fraction of the data that must be met in order for a
-- column to be identified as PII data.
pIIDetection_thresholdFraction :: Lens.Lens' PIIDetection (Prelude.Maybe Prelude.Double)
pIIDetection_thresholdFraction = Lens.lens (\PIIDetection' {thresholdFraction} -> thresholdFraction) (\s@PIIDetection' {} a -> s {thresholdFraction = a} :: PIIDetection)

-- | The name of the transform node.
pIIDetection_name :: Lens.Lens' PIIDetection Prelude.Text
pIIDetection_name = Lens.lens (\PIIDetection' {name} -> name) (\s@PIIDetection' {} a -> s {name = a} :: PIIDetection)

-- | The node ID inputs to the transform.
pIIDetection_inputs :: Lens.Lens' PIIDetection (Prelude.NonEmpty Prelude.Text)
pIIDetection_inputs = Lens.lens (\PIIDetection' {inputs} -> inputs) (\s@PIIDetection' {} a -> s {inputs = a} :: PIIDetection) Prelude.. Lens.coerced

-- | Indicates the type of PIIDetection transform.
pIIDetection_piiType :: Lens.Lens' PIIDetection PiiType
pIIDetection_piiType = Lens.lens (\PIIDetection' {piiType} -> piiType) (\s@PIIDetection' {} a -> s {piiType = a} :: PIIDetection)

-- | Indicates the types of entities the PIIDetection transform will identify
-- as PII data.
--
-- PII type entities include: PERSON_NAME, DATE, USA_SNN, EMAIL, USA_ITIN,
-- USA_PASSPORT_NUMBER, PHONE_NUMBER, BANK_ACCOUNT, IP_ADDRESS,
-- MAC_ADDRESS, USA_CPT_CODE, USA_HCPCS_CODE, USA_NATIONAL_DRUG_CODE,
-- USA_MEDICARE_BENEFICIARY_IDENTIFIER,
-- USA_HEALTH_INSURANCE_CLAIM_NUMBER,CREDIT_CARD,USA_NATIONAL_PROVIDER_IDENTIFIER,USA_DEA_NUMBER,USA_DRIVING_LICENSE
pIIDetection_entityTypesToDetect :: Lens.Lens' PIIDetection [Prelude.Text]
pIIDetection_entityTypesToDetect = Lens.lens (\PIIDetection' {entityTypesToDetect} -> entityTypesToDetect) (\s@PIIDetection' {} a -> s {entityTypesToDetect = a} :: PIIDetection) Prelude.. Lens.coerced

instance Core.FromJSON PIIDetection where
  parseJSON =
    Core.withObject
      "PIIDetection"
      ( \x ->
          PIIDetection'
            Prelude.<$> (x Core..:? "MaskValue")
            Prelude.<*> (x Core..:? "SampleFraction")
            Prelude.<*> (x Core..:? "OutputColumnName")
            Prelude.<*> (x Core..:? "ThresholdFraction")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Inputs")
            Prelude.<*> (x Core..: "PiiType")
            Prelude.<*> ( x Core..:? "EntityTypesToDetect"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PIIDetection where
  hashWithSalt _salt PIIDetection' {..} =
    _salt `Prelude.hashWithSalt` maskValue
      `Prelude.hashWithSalt` sampleFraction
      `Prelude.hashWithSalt` outputColumnName
      `Prelude.hashWithSalt` thresholdFraction
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` piiType
      `Prelude.hashWithSalt` entityTypesToDetect

instance Prelude.NFData PIIDetection where
  rnf PIIDetection' {..} =
    Prelude.rnf maskValue
      `Prelude.seq` Prelude.rnf sampleFraction
      `Prelude.seq` Prelude.rnf outputColumnName
      `Prelude.seq` Prelude.rnf thresholdFraction
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf piiType
      `Prelude.seq` Prelude.rnf entityTypesToDetect

instance Core.ToJSON PIIDetection where
  toJSON PIIDetection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaskValue" Core..=) Prelude.<$> maskValue,
            ("SampleFraction" Core..=)
              Prelude.<$> sampleFraction,
            ("OutputColumnName" Core..=)
              Prelude.<$> outputColumnName,
            ("ThresholdFraction" Core..=)
              Prelude.<$> thresholdFraction,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Inputs" Core..= inputs),
            Prelude.Just ("PiiType" Core..= piiType),
            Prelude.Just
              ("EntityTypesToDetect" Core..= entityTypesToDetect)
          ]
      )
