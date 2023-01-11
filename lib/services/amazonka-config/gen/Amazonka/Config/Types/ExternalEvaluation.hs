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
-- Module      : Amazonka.Config.Types.ExternalEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ExternalEvaluation where

import Amazonka.Config.Types.ComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies an Amazon Web Services resource and indicates whether it
-- complies with the Config rule that it was evaluated against.
--
-- /See:/ 'newExternalEvaluation' smart constructor.
data ExternalEvaluation = ExternalEvaluation'
  { -- | Supplementary information about the reason of compliance. For example,
    -- this task was completed on a specific date.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The evaluated compliance resource type. Config accepts @AWS::::Account@
    -- resource type.
    complianceResourceType :: Prelude.Text,
    -- | The evaluated compliance resource ID. Config accepts only Amazon Web
    -- Services account ID.
    complianceResourceId :: Prelude.Text,
    -- | The compliance of the Amazon Web Services resource. The valid values are
    -- @COMPLIANT, NON_COMPLIANT, @ and @NOT_APPLICABLE@.
    complianceType :: ComplianceType,
    -- | The time when the compliance was recorded.
    orderingTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'complianceResourceType', 'externalEvaluation_complianceResourceType' - The evaluated compliance resource type. Config accepts @AWS::::Account@
-- resource type.
--
-- 'complianceResourceId', 'externalEvaluation_complianceResourceId' - The evaluated compliance resource ID. Config accepts only Amazon Web
-- Services account ID.
--
-- 'complianceType', 'externalEvaluation_complianceType' - The compliance of the Amazon Web Services resource. The valid values are
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
          Data._Time Lens.# pOrderingTimestamp_
      }

-- | Supplementary information about the reason of compliance. For example,
-- this task was completed on a specific date.
externalEvaluation_annotation :: Lens.Lens' ExternalEvaluation (Prelude.Maybe Prelude.Text)
externalEvaluation_annotation = Lens.lens (\ExternalEvaluation' {annotation} -> annotation) (\s@ExternalEvaluation' {} a -> s {annotation = a} :: ExternalEvaluation)

-- | The evaluated compliance resource type. Config accepts @AWS::::Account@
-- resource type.
externalEvaluation_complianceResourceType :: Lens.Lens' ExternalEvaluation Prelude.Text
externalEvaluation_complianceResourceType = Lens.lens (\ExternalEvaluation' {complianceResourceType} -> complianceResourceType) (\s@ExternalEvaluation' {} a -> s {complianceResourceType = a} :: ExternalEvaluation)

-- | The evaluated compliance resource ID. Config accepts only Amazon Web
-- Services account ID.
externalEvaluation_complianceResourceId :: Lens.Lens' ExternalEvaluation Prelude.Text
externalEvaluation_complianceResourceId = Lens.lens (\ExternalEvaluation' {complianceResourceId} -> complianceResourceId) (\s@ExternalEvaluation' {} a -> s {complianceResourceId = a} :: ExternalEvaluation)

-- | The compliance of the Amazon Web Services resource. The valid values are
-- @COMPLIANT, NON_COMPLIANT, @ and @NOT_APPLICABLE@.
externalEvaluation_complianceType :: Lens.Lens' ExternalEvaluation ComplianceType
externalEvaluation_complianceType = Lens.lens (\ExternalEvaluation' {complianceType} -> complianceType) (\s@ExternalEvaluation' {} a -> s {complianceType = a} :: ExternalEvaluation)

-- | The time when the compliance was recorded.
externalEvaluation_orderingTimestamp :: Lens.Lens' ExternalEvaluation Prelude.UTCTime
externalEvaluation_orderingTimestamp = Lens.lens (\ExternalEvaluation' {orderingTimestamp} -> orderingTimestamp) (\s@ExternalEvaluation' {} a -> s {orderingTimestamp = a} :: ExternalEvaluation) Prelude.. Data._Time

instance Prelude.Hashable ExternalEvaluation where
  hashWithSalt _salt ExternalEvaluation' {..} =
    _salt `Prelude.hashWithSalt` annotation
      `Prelude.hashWithSalt` complianceResourceType
      `Prelude.hashWithSalt` complianceResourceId
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` orderingTimestamp

instance Prelude.NFData ExternalEvaluation where
  rnf ExternalEvaluation' {..} =
    Prelude.rnf annotation
      `Prelude.seq` Prelude.rnf complianceResourceType
      `Prelude.seq` Prelude.rnf complianceResourceId
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf orderingTimestamp

instance Data.ToJSON ExternalEvaluation where
  toJSON ExternalEvaluation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Annotation" Data..=) Prelude.<$> annotation,
            Prelude.Just
              ( "ComplianceResourceType"
                  Data..= complianceResourceType
              ),
            Prelude.Just
              ( "ComplianceResourceId"
                  Data..= complianceResourceId
              ),
            Prelude.Just
              ("ComplianceType" Data..= complianceType),
            Prelude.Just
              ("OrderingTimestamp" Data..= orderingTimestamp)
          ]
      )
