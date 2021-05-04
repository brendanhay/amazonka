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
-- Module      : Network.AWS.Config.Types.ExternalEvaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ExternalEvaluation where

import Network.AWS.Config.Types.ComplianceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies an AWS resource and indicates whether it complies with the
-- AWS Config rule that it was evaluated against.
--
-- /See:/ 'newExternalEvaluation' smart constructor.
data ExternalEvaluation = ExternalEvaluation'
  { -- | Supplementary information about the reason of compliance. For example,
    -- this task was completed on a specific date.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The evaluated compliance resource type. AWS Config accepts
    -- @AWS::::Account@ resource type.
    complianceResourceType :: Prelude.Text,
    -- | The evaluated compliance resource ID. AWS Config accepts only AWS
    -- account ID.
    complianceResourceId :: Prelude.Text,
    -- | The compliance of the AWS resource. The valid values are
    -- @COMPLIANT, NON_COMPLIANT, @ and @NOT_APPLICABLE@.
    complianceType :: ComplianceType,
    -- | The time when the compliance was recorded.
    orderingTimestamp :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExternalEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotation', 'externalEvaluation_annotation' - Supplementary information about the reason of compliance. For example,
-- this task was completed on a specific date.
--
-- 'complianceResourceType', 'externalEvaluation_complianceResourceType' - The evaluated compliance resource type. AWS Config accepts
-- @AWS::::Account@ resource type.
--
-- 'complianceResourceId', 'externalEvaluation_complianceResourceId' - The evaluated compliance resource ID. AWS Config accepts only AWS
-- account ID.
--
-- 'complianceType', 'externalEvaluation_complianceType' - The compliance of the AWS resource. The valid values are
-- @COMPLIANT, NON_COMPLIANT, @ and @NOT_APPLICABLE@.
--
-- 'orderingTimestamp', 'externalEvaluation_orderingTimestamp' - The time when the compliance was recorded.
newExternalEvaluation ::
  -- | 'complianceResourceType'
  Prelude.Text ->
  -- | 'complianceResourceId'
  Prelude.Text ->
  -- | 'complianceType'
  ComplianceType ->
  -- | 'orderingTimestamp'
  Prelude.UTCTime ->
  ExternalEvaluation
newExternalEvaluation
  pComplianceResourceType_
  pComplianceResourceId_
  pComplianceType_
  pOrderingTimestamp_ =
    ExternalEvaluation'
      { annotation = Prelude.Nothing,
        complianceResourceType = pComplianceResourceType_,
        complianceResourceId = pComplianceResourceId_,
        complianceType = pComplianceType_,
        orderingTimestamp =
          Prelude._Time Lens.# pOrderingTimestamp_
      }

-- | Supplementary information about the reason of compliance. For example,
-- this task was completed on a specific date.
externalEvaluation_annotation :: Lens.Lens' ExternalEvaluation (Prelude.Maybe Prelude.Text)
externalEvaluation_annotation = Lens.lens (\ExternalEvaluation' {annotation} -> annotation) (\s@ExternalEvaluation' {} a -> s {annotation = a} :: ExternalEvaluation)

-- | The evaluated compliance resource type. AWS Config accepts
-- @AWS::::Account@ resource type.
externalEvaluation_complianceResourceType :: Lens.Lens' ExternalEvaluation Prelude.Text
externalEvaluation_complianceResourceType = Lens.lens (\ExternalEvaluation' {complianceResourceType} -> complianceResourceType) (\s@ExternalEvaluation' {} a -> s {complianceResourceType = a} :: ExternalEvaluation)

-- | The evaluated compliance resource ID. AWS Config accepts only AWS
-- account ID.
externalEvaluation_complianceResourceId :: Lens.Lens' ExternalEvaluation Prelude.Text
externalEvaluation_complianceResourceId = Lens.lens (\ExternalEvaluation' {complianceResourceId} -> complianceResourceId) (\s@ExternalEvaluation' {} a -> s {complianceResourceId = a} :: ExternalEvaluation)

-- | The compliance of the AWS resource. The valid values are
-- @COMPLIANT, NON_COMPLIANT, @ and @NOT_APPLICABLE@.
externalEvaluation_complianceType :: Lens.Lens' ExternalEvaluation ComplianceType
externalEvaluation_complianceType = Lens.lens (\ExternalEvaluation' {complianceType} -> complianceType) (\s@ExternalEvaluation' {} a -> s {complianceType = a} :: ExternalEvaluation)

-- | The time when the compliance was recorded.
externalEvaluation_orderingTimestamp :: Lens.Lens' ExternalEvaluation Prelude.UTCTime
externalEvaluation_orderingTimestamp = Lens.lens (\ExternalEvaluation' {orderingTimestamp} -> orderingTimestamp) (\s@ExternalEvaluation' {} a -> s {orderingTimestamp = a} :: ExternalEvaluation) Prelude.. Prelude._Time

instance Prelude.Hashable ExternalEvaluation

instance Prelude.NFData ExternalEvaluation

instance Prelude.ToJSON ExternalEvaluation where
  toJSON ExternalEvaluation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Annotation" Prelude..=) Prelude.<$> annotation,
            Prelude.Just
              ( "ComplianceResourceType"
                  Prelude..= complianceResourceType
              ),
            Prelude.Just
              ( "ComplianceResourceId"
                  Prelude..= complianceResourceId
              ),
            Prelude.Just
              ("ComplianceType" Prelude..= complianceType),
            Prelude.Just
              ("OrderingTimestamp" Prelude..= orderingTimestamp)
          ]
      )
