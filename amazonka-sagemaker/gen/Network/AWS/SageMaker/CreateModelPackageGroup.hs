{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateModelPackageGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model group. A model group contains a group of model versions.
module Network.AWS.SageMaker.CreateModelPackageGroup
  ( -- * Creating a Request
    CreateModelPackageGroup (..),
    newCreateModelPackageGroup,

    -- * Request Lenses
    createModelPackageGroup_modelPackageGroupDescription,
    createModelPackageGroup_tags,
    createModelPackageGroup_modelPackageGroupName,

    -- * Destructuring the Response
    CreateModelPackageGroupResponse (..),
    newCreateModelPackageGroupResponse,

    -- * Response Lenses
    createModelPackageGroupResponse_httpStatus,
    createModelPackageGroupResponse_modelPackageGroupArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateModelPackageGroup' smart constructor.
data CreateModelPackageGroup = CreateModelPackageGroup'
  { -- | A description for the model group.
    modelPackageGroupDescription :: Core.Maybe Core.Text,
    -- | A list of key value pairs associated with the model group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference Guide/.
    tags :: Core.Maybe [Tag],
    -- | The name of the model group.
    modelPackageGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModelPackageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupDescription', 'createModelPackageGroup_modelPackageGroupDescription' - A description for the model group.
--
-- 'tags', 'createModelPackageGroup_tags' - A list of key value pairs associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
--
-- 'modelPackageGroupName', 'createModelPackageGroup_modelPackageGroupName' - The name of the model group.
newCreateModelPackageGroup ::
  -- | 'modelPackageGroupName'
  Core.Text ->
  CreateModelPackageGroup
newCreateModelPackageGroup pModelPackageGroupName_ =
  CreateModelPackageGroup'
    { modelPackageGroupDescription =
        Core.Nothing,
      tags = Core.Nothing,
      modelPackageGroupName = pModelPackageGroupName_
    }

-- | A description for the model group.
createModelPackageGroup_modelPackageGroupDescription :: Lens.Lens' CreateModelPackageGroup (Core.Maybe Core.Text)
createModelPackageGroup_modelPackageGroupDescription = Lens.lens (\CreateModelPackageGroup' {modelPackageGroupDescription} -> modelPackageGroupDescription) (\s@CreateModelPackageGroup' {} a -> s {modelPackageGroupDescription = a} :: CreateModelPackageGroup)

-- | A list of key value pairs associated with the model group. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
createModelPackageGroup_tags :: Lens.Lens' CreateModelPackageGroup (Core.Maybe [Tag])
createModelPackageGroup_tags = Lens.lens (\CreateModelPackageGroup' {tags} -> tags) (\s@CreateModelPackageGroup' {} a -> s {tags = a} :: CreateModelPackageGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the model group.
createModelPackageGroup_modelPackageGroupName :: Lens.Lens' CreateModelPackageGroup Core.Text
createModelPackageGroup_modelPackageGroupName = Lens.lens (\CreateModelPackageGroup' {modelPackageGroupName} -> modelPackageGroupName) (\s@CreateModelPackageGroup' {} a -> s {modelPackageGroupName = a} :: CreateModelPackageGroup)

instance Core.AWSRequest CreateModelPackageGroup where
  type
    AWSResponse CreateModelPackageGroup =
      CreateModelPackageGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelPackageGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ModelPackageGroupArn")
      )

instance Core.Hashable CreateModelPackageGroup

instance Core.NFData CreateModelPackageGroup

instance Core.ToHeaders CreateModelPackageGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateModelPackageGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateModelPackageGroup where
  toJSON CreateModelPackageGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ModelPackageGroupDescription" Core..=)
              Core.<$> modelPackageGroupDescription,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just
              ( "ModelPackageGroupName"
                  Core..= modelPackageGroupName
              )
          ]
      )

instance Core.ToPath CreateModelPackageGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateModelPackageGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateModelPackageGroupResponse' smart constructor.
data CreateModelPackageGroupResponse = CreateModelPackageGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model group.
    modelPackageGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateModelPackageGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelPackageGroupResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageGroupArn', 'createModelPackageGroupResponse_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model group.
newCreateModelPackageGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'modelPackageGroupArn'
  Core.Text ->
  CreateModelPackageGroupResponse
newCreateModelPackageGroupResponse
  pHttpStatus_
  pModelPackageGroupArn_ =
    CreateModelPackageGroupResponse'
      { httpStatus =
          pHttpStatus_,
        modelPackageGroupArn =
          pModelPackageGroupArn_
      }

-- | The response's http status code.
createModelPackageGroupResponse_httpStatus :: Lens.Lens' CreateModelPackageGroupResponse Core.Int
createModelPackageGroupResponse_httpStatus = Lens.lens (\CreateModelPackageGroupResponse' {httpStatus} -> httpStatus) (\s@CreateModelPackageGroupResponse' {} a -> s {httpStatus = a} :: CreateModelPackageGroupResponse)

-- | The Amazon Resource Name (ARN) of the model group.
createModelPackageGroupResponse_modelPackageGroupArn :: Lens.Lens' CreateModelPackageGroupResponse Core.Text
createModelPackageGroupResponse_modelPackageGroupArn = Lens.lens (\CreateModelPackageGroupResponse' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@CreateModelPackageGroupResponse' {} a -> s {modelPackageGroupArn = a} :: CreateModelPackageGroupResponse)

instance Core.NFData CreateModelPackageGroupResponse
