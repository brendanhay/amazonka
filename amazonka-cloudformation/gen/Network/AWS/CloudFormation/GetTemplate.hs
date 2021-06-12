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
-- Module      : Network.AWS.CloudFormation.GetTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template body for a specified stack. You can get the
-- template for running or deleted stacks.
--
-- For deleted stacks, GetTemplate returns the template for up to 90 days
-- after the stack has been deleted.
--
-- If the template does not exist, a @ValidationError@ is returned.
module Network.AWS.CloudFormation.GetTemplate
  ( -- * Creating a Request
    GetTemplate (..),
    newGetTemplate,

    -- * Request Lenses
    getTemplate_templateStage,
    getTemplate_stackName,
    getTemplate_changeSetName,

    -- * Destructuring the Response
    GetTemplateResponse (..),
    newGetTemplateResponse,

    -- * Response Lenses
    getTemplateResponse_stagesAvailable,
    getTemplateResponse_templateBody,
    getTemplateResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for a GetTemplate action.
--
-- /See:/ 'newGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | For templates that include transforms, the stage of the template that
    -- AWS CloudFormation returns. To get the user-submitted template, specify
    -- @Original@. To get the template after AWS CloudFormation has processed
    -- all transforms, specify @Processed@.
    --
    -- If the template doesn\'t include transforms, @Original@ and @Processed@
    -- return the same template. By default, AWS CloudFormation specifies
    -- @Original@.
    templateStage :: Core.Maybe TemplateStage,
    -- | The name or the unique stack ID that is associated with the stack, which
    -- are not always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    stackName :: Core.Maybe Core.Text,
    -- | The name or Amazon Resource Name (ARN) of a change set for which AWS
    -- CloudFormation returns the associated template. If you specify a name,
    -- you must also specify the @StackName@.
    changeSetName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateStage', 'getTemplate_templateStage' - For templates that include transforms, the stage of the template that
-- AWS CloudFormation returns. To get the user-submitted template, specify
-- @Original@. To get the template after AWS CloudFormation has processed
-- all transforms, specify @Processed@.
--
-- If the template doesn\'t include transforms, @Original@ and @Processed@
-- return the same template. By default, AWS CloudFormation specifies
-- @Original@.
--
-- 'stackName', 'getTemplate_stackName' - The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
--
-- 'changeSetName', 'getTemplate_changeSetName' - The name or Amazon Resource Name (ARN) of a change set for which AWS
-- CloudFormation returns the associated template. If you specify a name,
-- you must also specify the @StackName@.
newGetTemplate ::
  GetTemplate
newGetTemplate =
  GetTemplate'
    { templateStage = Core.Nothing,
      stackName = Core.Nothing,
      changeSetName = Core.Nothing
    }

-- | For templates that include transforms, the stage of the template that
-- AWS CloudFormation returns. To get the user-submitted template, specify
-- @Original@. To get the template after AWS CloudFormation has processed
-- all transforms, specify @Processed@.
--
-- If the template doesn\'t include transforms, @Original@ and @Processed@
-- return the same template. By default, AWS CloudFormation specifies
-- @Original@.
getTemplate_templateStage :: Lens.Lens' GetTemplate (Core.Maybe TemplateStage)
getTemplate_templateStage = Lens.lens (\GetTemplate' {templateStage} -> templateStage) (\s@GetTemplate' {} a -> s {templateStage = a} :: GetTemplate)

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
getTemplate_stackName :: Lens.Lens' GetTemplate (Core.Maybe Core.Text)
getTemplate_stackName = Lens.lens (\GetTemplate' {stackName} -> stackName) (\s@GetTemplate' {} a -> s {stackName = a} :: GetTemplate)

-- | The name or Amazon Resource Name (ARN) of a change set for which AWS
-- CloudFormation returns the associated template. If you specify a name,
-- you must also specify the @StackName@.
getTemplate_changeSetName :: Lens.Lens' GetTemplate (Core.Maybe Core.Text)
getTemplate_changeSetName = Lens.lens (\GetTemplate' {changeSetName} -> changeSetName) (\s@GetTemplate' {} a -> s {changeSetName = a} :: GetTemplate)

instance Core.AWSRequest GetTemplate where
  type AWSResponse GetTemplate = GetTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Core.<$> ( x Core..@? "StagesAvailable" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "TemplateBody")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTemplate

instance Core.NFData GetTemplate

instance Core.ToHeaders GetTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetTemplate where
  toPath = Core.const "/"

instance Core.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Core.mconcat
      [ "Action" Core.=: ("GetTemplate" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "TemplateStage" Core.=: templateStage,
        "StackName" Core.=: stackName,
        "ChangeSetName" Core.=: changeSetName
      ]

-- | The output for GetTemplate action.
--
-- /See:/ 'newGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { -- | The stage of the template that you can retrieve. For stacks, the
    -- @Original@ and @Processed@ templates are always available. For change
    -- sets, the @Original@ template is always available. After AWS
    -- CloudFormation finishes creating the change set, the @Processed@
    -- template becomes available.
    stagesAvailable :: Core.Maybe [TemplateStage],
    -- | Structure containing the template body. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.)
    --
    -- AWS CloudFormation returns the same template that was used when the
    -- stack was created.
    templateBody :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stagesAvailable', 'getTemplateResponse_stagesAvailable' - The stage of the template that you can retrieve. For stacks, the
-- @Original@ and @Processed@ templates are always available. For change
-- sets, the @Original@ template is always available. After AWS
-- CloudFormation finishes creating the change set, the @Processed@
-- template becomes available.
--
-- 'templateBody', 'getTemplateResponse_templateBody' - Structure containing the template body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- AWS CloudFormation returns the same template that was used when the
-- stack was created.
--
-- 'httpStatus', 'getTemplateResponse_httpStatus' - The response's http status code.
newGetTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTemplateResponse
newGetTemplateResponse pHttpStatus_ =
  GetTemplateResponse'
    { stagesAvailable =
        Core.Nothing,
      templateBody = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stage of the template that you can retrieve. For stacks, the
-- @Original@ and @Processed@ templates are always available. For change
-- sets, the @Original@ template is always available. After AWS
-- CloudFormation finishes creating the change set, the @Processed@
-- template becomes available.
getTemplateResponse_stagesAvailable :: Lens.Lens' GetTemplateResponse (Core.Maybe [TemplateStage])
getTemplateResponse_stagesAvailable = Lens.lens (\GetTemplateResponse' {stagesAvailable} -> stagesAvailable) (\s@GetTemplateResponse' {} a -> s {stagesAvailable = a} :: GetTemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | Structure containing the template body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- AWS CloudFormation returns the same template that was used when the
-- stack was created.
getTemplateResponse_templateBody :: Lens.Lens' GetTemplateResponse (Core.Maybe Core.Text)
getTemplateResponse_templateBody = Lens.lens (\GetTemplateResponse' {templateBody} -> templateBody) (\s@GetTemplateResponse' {} a -> s {templateBody = a} :: GetTemplateResponse)

-- | The response's http status code.
getTemplateResponse_httpStatus :: Lens.Lens' GetTemplateResponse Core.Int
getTemplateResponse_httpStatus = Lens.lens (\GetTemplateResponse' {httpStatus} -> httpStatus) (\s@GetTemplateResponse' {} a -> s {httpStatus = a} :: GetTemplateResponse)

instance Core.NFData GetTemplateResponse
