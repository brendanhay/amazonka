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
-- Module      : Amazonka.SageMaker.Types.ModelPackageGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelPackageGroupStatus
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | A group of versioned models in the model registry.
--
-- /See:/ 'newModelPackageGroup' smart constructor.
data ModelPackageGroup = ModelPackageGroup'
  { -- | A list of the tags associated with the model group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the model group.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the model group. This can be one of the following values.
    --
    -- -   @PENDING@ - The model group is pending being created.
    --
    -- -   @IN_PROGRESS@ - The model group is in the process of being created.
    --
    -- -   @COMPLETED@ - The model group was successfully created.
    --
    -- -   @FAILED@ - The model group failed.
    --
    -- -   @DELETING@ - The model group is in the process of being deleted.
    --
    -- -   @DELETE_FAILED@ - SageMaker failed to delete the model group.
    modelPackageGroupStatus :: Prelude.Maybe ModelPackageGroupStatus,
    -- | The description for the model group.
    modelPackageGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The time that the model group was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    createdBy :: Prelude.Maybe UserContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'modelPackageGroup_tags' - A list of the tags associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'modelPackageGroupName', 'modelPackageGroup_modelPackageGroupName' - The name of the model group.
--
-- 'modelPackageGroupArn', 'modelPackageGroup_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model group.
--
-- 'modelPackageGroupStatus', 'modelPackageGroup_modelPackageGroupStatus' - The status of the model group. This can be one of the following values.
--
-- -   @PENDING@ - The model group is pending being created.
--
-- -   @IN_PROGRESS@ - The model group is in the process of being created.
--
-- -   @COMPLETED@ - The model group was successfully created.
--
-- -   @FAILED@ - The model group failed.
--
-- -   @DELETING@ - The model group is in the process of being deleted.
--
-- -   @DELETE_FAILED@ - SageMaker failed to delete the model group.
--
-- 'modelPackageGroupDescription', 'modelPackageGroup_modelPackageGroupDescription' - The description for the model group.
--
-- 'creationTime', 'modelPackageGroup_creationTime' - The time that the model group was created.
--
-- 'createdBy', 'modelPackageGroup_createdBy' - Undocumented member.
newModelPackageGroup ::
  ModelPackageGroup
newModelPackageGroup =
  ModelPackageGroup'
    { tags = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing,
      modelPackageGroupArn = Prelude.Nothing,
      modelPackageGroupStatus = Prelude.Nothing,
      modelPackageGroupDescription = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      createdBy = Prelude.Nothing
    }

-- | A list of the tags associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
modelPackageGroup_tags :: Lens.Lens' ModelPackageGroup (Prelude.Maybe [Tag])
modelPackageGroup_tags = Lens.lens (\ModelPackageGroup' {tags} -> tags) (\s@ModelPackageGroup' {} a -> s {tags = a} :: ModelPackageGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the model group.
modelPackageGroup_modelPackageGroupName :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupName = Lens.lens (\ModelPackageGroup' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupName = a} :: ModelPackageGroup)

-- | The Amazon Resource Name (ARN) of the model group.
modelPackageGroup_modelPackageGroupArn :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupArn = Lens.lens (\ModelPackageGroup' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupArn = a} :: ModelPackageGroup)

-- | The status of the model group. This can be one of the following values.
--
-- -   @PENDING@ - The model group is pending being created.
--
-- -   @IN_PROGRESS@ - The model group is in the process of being created.
--
-- -   @COMPLETED@ - The model group was successfully created.
--
-- -   @FAILED@ - The model group failed.
--
-- -   @DELETING@ - The model group is in the process of being deleted.
--
-- -   @DELETE_FAILED@ - SageMaker failed to delete the model group.
modelPackageGroup_modelPackageGroupStatus :: Lens.Lens' ModelPackageGroup (Prelude.Maybe ModelPackageGroupStatus)
modelPackageGroup_modelPackageGroupStatus = Lens.lens (\ModelPackageGroup' {modelPackageGroupStatus} -> modelPackageGroupStatus) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupStatus = a} :: ModelPackageGroup)

-- | The description for the model group.
modelPackageGroup_modelPackageGroupDescription :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupDescription = Lens.lens (\ModelPackageGroup' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupDescription = a} :: ModelPackageGroup)

-- | The time that the model group was created.
modelPackageGroup_creationTime :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.UTCTime)
modelPackageGroup_creationTime = Lens.lens (\ModelPackageGroup' {creationTime} -> creationTime) (\s@ModelPackageGroup' {} a -> s {creationTime = a} :: ModelPackageGroup) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
modelPackageGroup_createdBy :: Lens.Lens' ModelPackageGroup (Prelude.Maybe UserContext)
modelPackageGroup_createdBy = Lens.lens (\ModelPackageGroup' {createdBy} -> createdBy) (\s@ModelPackageGroup' {} a -> s {createdBy = a} :: ModelPackageGroup)

instance Core.FromJSON ModelPackageGroup where
  parseJSON =
    Core.withObject
      "ModelPackageGroup"
      ( \x ->
          ModelPackageGroup'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ModelPackageGroupName")
            Prelude.<*> (x Core..:? "ModelPackageGroupArn")
            Prelude.<*> (x Core..:? "ModelPackageGroupStatus")
            Prelude.<*> (x Core..:? "ModelPackageGroupDescription")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "CreatedBy")
      )

instance Prelude.Hashable ModelPackageGroup where
  hashWithSalt _salt ModelPackageGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` modelPackageGroupArn
      `Prelude.hashWithSalt` modelPackageGroupStatus
      `Prelude.hashWithSalt` modelPackageGroupDescription
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` createdBy

instance Prelude.NFData ModelPackageGroup where
  rnf ModelPackageGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageGroupArn
      `Prelude.seq` Prelude.rnf modelPackageGroupStatus
      `Prelude.seq` Prelude.rnf modelPackageGroupDescription
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf createdBy
