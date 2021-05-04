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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ModelPackageGroupStatus
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | A group of versioned models in the model registry.
--
-- /See:/ 'newModelPackageGroup' smart constructor.
data ModelPackageGroup = ModelPackageGroup'
  { -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the model group was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The description for the model group.
    modelPackageGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of the tags associated with the model group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
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
    createdBy :: Prelude.Maybe UserContext,
    -- | The name of the model group.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupArn', 'modelPackageGroup_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model group.
--
-- 'creationTime', 'modelPackageGroup_creationTime' - The time that the model group was created.
--
-- 'modelPackageGroupDescription', 'modelPackageGroup_modelPackageGroupDescription' - The description for the model group.
--
-- 'tags', 'modelPackageGroup_tags' - A list of the tags associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
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
-- 'createdBy', 'modelPackageGroup_createdBy' - Undocumented member.
--
-- 'modelPackageGroupName', 'modelPackageGroup_modelPackageGroupName' - The name of the model group.
newModelPackageGroup ::
  ModelPackageGroup
newModelPackageGroup =
  ModelPackageGroup'
    { modelPackageGroupArn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      modelPackageGroupDescription = Prelude.Nothing,
      tags = Prelude.Nothing,
      modelPackageGroupStatus = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the model group.
modelPackageGroup_modelPackageGroupArn :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupArn = Lens.lens (\ModelPackageGroup' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupArn = a} :: ModelPackageGroup)

-- | The time that the model group was created.
modelPackageGroup_creationTime :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.UTCTime)
modelPackageGroup_creationTime = Lens.lens (\ModelPackageGroup' {creationTime} -> creationTime) (\s@ModelPackageGroup' {} a -> s {creationTime = a} :: ModelPackageGroup) Prelude.. Lens.mapping Prelude._Time

-- | The description for the model group.
modelPackageGroup_modelPackageGroupDescription :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupDescription = Lens.lens (\ModelPackageGroup' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupDescription = a} :: ModelPackageGroup)

-- | A list of the tags associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
modelPackageGroup_tags :: Lens.Lens' ModelPackageGroup (Prelude.Maybe [Tag])
modelPackageGroup_tags = Lens.lens (\ModelPackageGroup' {tags} -> tags) (\s@ModelPackageGroup' {} a -> s {tags = a} :: ModelPackageGroup) Prelude.. Lens.mapping Prelude._Coerce

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

-- | Undocumented member.
modelPackageGroup_createdBy :: Lens.Lens' ModelPackageGroup (Prelude.Maybe UserContext)
modelPackageGroup_createdBy = Lens.lens (\ModelPackageGroup' {createdBy} -> createdBy) (\s@ModelPackageGroup' {} a -> s {createdBy = a} :: ModelPackageGroup)

-- | The name of the model group.
modelPackageGroup_modelPackageGroupName :: Lens.Lens' ModelPackageGroup (Prelude.Maybe Prelude.Text)
modelPackageGroup_modelPackageGroupName = Lens.lens (\ModelPackageGroup' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackageGroup' {} a -> s {modelPackageGroupName = a} :: ModelPackageGroup)

instance Prelude.FromJSON ModelPackageGroup where
  parseJSON =
    Prelude.withObject
      "ModelPackageGroup"
      ( \x ->
          ModelPackageGroup'
            Prelude.<$> (x Prelude..:? "ModelPackageGroupArn")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ModelPackageGroupDescription")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "ModelPackageGroupStatus")
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "ModelPackageGroupName")
      )

instance Prelude.Hashable ModelPackageGroup

instance Prelude.NFData ModelPackageGroup
