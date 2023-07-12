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
-- Module      : Amazonka.SageMaker.Types.InstanceGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InstanceGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrainingInstanceType

-- | Defines an instance group for heterogeneous cluster training. When
-- requesting a training job using the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateTrainingJob.html CreateTrainingJob>
-- API, you can configure multiple instance groups .
--
-- /See:/ 'newInstanceGroup' smart constructor.
data InstanceGroup = InstanceGroup'
  { -- | Specifies the instance type of the instance group.
    instanceType :: TrainingInstanceType,
    -- | Specifies the number of instances of the instance group.
    instanceCount :: Prelude.Natural,
    -- | Specifies the name of the instance group.
    instanceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instanceGroup_instanceType' - Specifies the instance type of the instance group.
--
-- 'instanceCount', 'instanceGroup_instanceCount' - Specifies the number of instances of the instance group.
--
-- 'instanceGroupName', 'instanceGroup_instanceGroupName' - Specifies the name of the instance group.
newInstanceGroup ::
  -- | 'instanceType'
  TrainingInstanceType ->
  -- | 'instanceCount'
  Prelude.Natural ->
  -- | 'instanceGroupName'
  Prelude.Text ->
  InstanceGroup
newInstanceGroup
  pInstanceType_
  pInstanceCount_
  pInstanceGroupName_ =
    InstanceGroup'
      { instanceType = pInstanceType_,
        instanceCount = pInstanceCount_,
        instanceGroupName = pInstanceGroupName_
      }

-- | Specifies the instance type of the instance group.
instanceGroup_instanceType :: Lens.Lens' InstanceGroup TrainingInstanceType
instanceGroup_instanceType = Lens.lens (\InstanceGroup' {instanceType} -> instanceType) (\s@InstanceGroup' {} a -> s {instanceType = a} :: InstanceGroup)

-- | Specifies the number of instances of the instance group.
instanceGroup_instanceCount :: Lens.Lens' InstanceGroup Prelude.Natural
instanceGroup_instanceCount = Lens.lens (\InstanceGroup' {instanceCount} -> instanceCount) (\s@InstanceGroup' {} a -> s {instanceCount = a} :: InstanceGroup)

-- | Specifies the name of the instance group.
instanceGroup_instanceGroupName :: Lens.Lens' InstanceGroup Prelude.Text
instanceGroup_instanceGroupName = Lens.lens (\InstanceGroup' {instanceGroupName} -> instanceGroupName) (\s@InstanceGroup' {} a -> s {instanceGroupName = a} :: InstanceGroup)

instance Data.FromJSON InstanceGroup where
  parseJSON =
    Data.withObject
      "InstanceGroup"
      ( \x ->
          InstanceGroup'
            Prelude.<$> (x Data..: "InstanceType")
            Prelude.<*> (x Data..: "InstanceCount")
            Prelude.<*> (x Data..: "InstanceGroupName")
      )

instance Prelude.Hashable InstanceGroup where
  hashWithSalt _salt InstanceGroup' {..} =
    _salt
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceGroupName

instance Prelude.NFData InstanceGroup where
  rnf InstanceGroup' {..} =
    Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceGroupName

instance Data.ToJSON InstanceGroup where
  toJSON InstanceGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceType" Data..= instanceType),
            Prelude.Just ("InstanceCount" Data..= instanceCount),
            Prelude.Just
              ("InstanceGroupName" Data..= instanceGroupName)
          ]
      )
