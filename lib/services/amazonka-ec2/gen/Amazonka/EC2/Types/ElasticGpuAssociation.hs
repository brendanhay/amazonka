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
-- Module      : Amazonka.EC2.Types.ElasticGpuAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ElasticGpuAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the association between an instance and an Elastic Graphics
-- accelerator.
--
-- /See:/ 'newElasticGpuAssociation' smart constructor.
data ElasticGpuAssociation = ElasticGpuAssociation'
  { -- | The ID of the association.
    elasticGpuAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association between the instance and the Elastic
    -- Graphics accelerator.
    elasticGpuAssociationState :: Prelude.Maybe Prelude.Text,
    -- | The time the Elastic Graphics accelerator was associated with the
    -- instance.
    elasticGpuAssociationTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Elastic Graphics accelerator.
    elasticGpuId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticGpuAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticGpuAssociationId', 'elasticGpuAssociation_elasticGpuAssociationId' - The ID of the association.
--
-- 'elasticGpuAssociationState', 'elasticGpuAssociation_elasticGpuAssociationState' - The state of the association between the instance and the Elastic
-- Graphics accelerator.
--
-- 'elasticGpuAssociationTime', 'elasticGpuAssociation_elasticGpuAssociationTime' - The time the Elastic Graphics accelerator was associated with the
-- instance.
--
-- 'elasticGpuId', 'elasticGpuAssociation_elasticGpuId' - The ID of the Elastic Graphics accelerator.
newElasticGpuAssociation ::
  ElasticGpuAssociation
newElasticGpuAssociation =
  ElasticGpuAssociation'
    { elasticGpuAssociationId =
        Prelude.Nothing,
      elasticGpuAssociationState = Prelude.Nothing,
      elasticGpuAssociationTime = Prelude.Nothing,
      elasticGpuId = Prelude.Nothing
    }

-- | The ID of the association.
elasticGpuAssociation_elasticGpuAssociationId :: Lens.Lens' ElasticGpuAssociation (Prelude.Maybe Prelude.Text)
elasticGpuAssociation_elasticGpuAssociationId = Lens.lens (\ElasticGpuAssociation' {elasticGpuAssociationId} -> elasticGpuAssociationId) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuAssociationId = a} :: ElasticGpuAssociation)

-- | The state of the association between the instance and the Elastic
-- Graphics accelerator.
elasticGpuAssociation_elasticGpuAssociationState :: Lens.Lens' ElasticGpuAssociation (Prelude.Maybe Prelude.Text)
elasticGpuAssociation_elasticGpuAssociationState = Lens.lens (\ElasticGpuAssociation' {elasticGpuAssociationState} -> elasticGpuAssociationState) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuAssociationState = a} :: ElasticGpuAssociation)

-- | The time the Elastic Graphics accelerator was associated with the
-- instance.
elasticGpuAssociation_elasticGpuAssociationTime :: Lens.Lens' ElasticGpuAssociation (Prelude.Maybe Prelude.Text)
elasticGpuAssociation_elasticGpuAssociationTime = Lens.lens (\ElasticGpuAssociation' {elasticGpuAssociationTime} -> elasticGpuAssociationTime) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuAssociationTime = a} :: ElasticGpuAssociation)

-- | The ID of the Elastic Graphics accelerator.
elasticGpuAssociation_elasticGpuId :: Lens.Lens' ElasticGpuAssociation (Prelude.Maybe Prelude.Text)
elasticGpuAssociation_elasticGpuId = Lens.lens (\ElasticGpuAssociation' {elasticGpuId} -> elasticGpuId) (\s@ElasticGpuAssociation' {} a -> s {elasticGpuId = a} :: ElasticGpuAssociation)

instance Data.FromXML ElasticGpuAssociation where
  parseXML x =
    ElasticGpuAssociation'
      Prelude.<$> (x Data..@? "elasticGpuAssociationId")
      Prelude.<*> (x Data..@? "elasticGpuAssociationState")
      Prelude.<*> (x Data..@? "elasticGpuAssociationTime")
      Prelude.<*> (x Data..@? "elasticGpuId")

instance Prelude.Hashable ElasticGpuAssociation where
  hashWithSalt _salt ElasticGpuAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` elasticGpuAssociationId
      `Prelude.hashWithSalt` elasticGpuAssociationState
      `Prelude.hashWithSalt` elasticGpuAssociationTime
      `Prelude.hashWithSalt` elasticGpuId

instance Prelude.NFData ElasticGpuAssociation where
  rnf ElasticGpuAssociation' {..} =
    Prelude.rnf elasticGpuAssociationId
      `Prelude.seq` Prelude.rnf elasticGpuAssociationState
      `Prelude.seq` Prelude.rnf elasticGpuAssociationTime
      `Prelude.seq` Prelude.rnf elasticGpuId
