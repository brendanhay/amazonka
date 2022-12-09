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
-- Module      : Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the association between an instance and an elastic inference
-- accelerator.
--
-- /See:/ 'newElasticInferenceAcceleratorAssociation' smart constructor.
data ElasticInferenceAcceleratorAssociation = ElasticInferenceAcceleratorAssociation'
  { -- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
    elasticInferenceAcceleratorArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    elasticInferenceAcceleratorAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the elastic inference accelerator.
    elasticInferenceAcceleratorAssociationState :: Prelude.Maybe Prelude.Text,
    -- | The time at which the elastic inference accelerator is associated with
    -- an instance.
    elasticInferenceAcceleratorAssociationTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticInferenceAcceleratorAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticInferenceAcceleratorArn', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn' - The Amazon Resource Name (ARN) of the elastic inference accelerator.
--
-- 'elasticInferenceAcceleratorAssociationId', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId' - The ID of the association.
--
-- 'elasticInferenceAcceleratorAssociationState', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState' - The state of the elastic inference accelerator.
--
-- 'elasticInferenceAcceleratorAssociationTime', 'elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime' - The time at which the elastic inference accelerator is associated with
-- an instance.
newElasticInferenceAcceleratorAssociation ::
  ElasticInferenceAcceleratorAssociation
newElasticInferenceAcceleratorAssociation =
  ElasticInferenceAcceleratorAssociation'
    { elasticInferenceAcceleratorArn =
        Prelude.Nothing,
      elasticInferenceAcceleratorAssociationId =
        Prelude.Nothing,
      elasticInferenceAcceleratorAssociationState =
        Prelude.Nothing,
      elasticInferenceAcceleratorAssociationTime =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Prelude.Maybe Prelude.Text)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorArn = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorArn} -> elasticInferenceAcceleratorArn) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorArn = a} :: ElasticInferenceAcceleratorAssociation)

-- | The ID of the association.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Prelude.Maybe Prelude.Text)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationId = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorAssociationId} -> elasticInferenceAcceleratorAssociationId) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorAssociationId = a} :: ElasticInferenceAcceleratorAssociation)

-- | The state of the elastic inference accelerator.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Prelude.Maybe Prelude.Text)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationState = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorAssociationState} -> elasticInferenceAcceleratorAssociationState) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorAssociationState = a} :: ElasticInferenceAcceleratorAssociation)

-- | The time at which the elastic inference accelerator is associated with
-- an instance.
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime :: Lens.Lens' ElasticInferenceAcceleratorAssociation (Prelude.Maybe Prelude.UTCTime)
elasticInferenceAcceleratorAssociation_elasticInferenceAcceleratorAssociationTime = Lens.lens (\ElasticInferenceAcceleratorAssociation' {elasticInferenceAcceleratorAssociationTime} -> elasticInferenceAcceleratorAssociationTime) (\s@ElasticInferenceAcceleratorAssociation' {} a -> s {elasticInferenceAcceleratorAssociationTime = a} :: ElasticInferenceAcceleratorAssociation) Prelude.. Lens.mapping Data._Time

instance
  Data.FromXML
    ElasticInferenceAcceleratorAssociation
  where
  parseXML x =
    ElasticInferenceAcceleratorAssociation'
      Prelude.<$> (x Data..@? "elasticInferenceAcceleratorArn")
      Prelude.<*> ( x
                      Data..@? "elasticInferenceAcceleratorAssociationId"
                  )
      Prelude.<*> ( x
                      Data..@? "elasticInferenceAcceleratorAssociationState"
                  )
      Prelude.<*> ( x
                      Data..@? "elasticInferenceAcceleratorAssociationTime"
                  )

instance
  Prelude.Hashable
    ElasticInferenceAcceleratorAssociation
  where
  hashWithSalt
    _salt
    ElasticInferenceAcceleratorAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` elasticInferenceAcceleratorArn
        `Prelude.hashWithSalt` elasticInferenceAcceleratorAssociationId
        `Prelude.hashWithSalt` elasticInferenceAcceleratorAssociationState
        `Prelude.hashWithSalt` elasticInferenceAcceleratorAssociationTime

instance
  Prelude.NFData
    ElasticInferenceAcceleratorAssociation
  where
  rnf ElasticInferenceAcceleratorAssociation' {..} =
    Prelude.rnf elasticInferenceAcceleratorArn
      `Prelude.seq` Prelude.rnf elasticInferenceAcceleratorAssociationId
      `Prelude.seq` Prelude.rnf
        elasticInferenceAcceleratorAssociationState
      `Prelude.seq` Prelude.rnf
        elasticInferenceAcceleratorAssociationTime
