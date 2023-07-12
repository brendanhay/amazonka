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
-- Module      : Amazonka.CloudFormation.GetTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template body for a specified stack. You can get the
-- template for running or deleted stacks.
--
-- For deleted stacks, @GetTemplate@ returns the template for up to 90 days
-- after the stack has been deleted.
--
-- If the template doesn\'t exist, a @ValidationError@ is returned.
module Amazonka.CloudFormation.GetTemplate
  ( -- * Creating a Request
    GetTemplate (..),
    newGetTemplate,

    -- * Request Lenses
    getTemplate_changeSetName,
    getTemplate_stackName,
    getTemplate_templateStage,

    -- * Destructuring the Response
    GetTemplateResponse (..),
    newGetTemplateResponse,

    -- * Response Lenses
    getTemplateResponse_stagesAvailable,
    getTemplateResponse_templateBody,
    getTemplateResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for a GetTemplate action.
--
-- /See:/ 'newGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | The name or Amazon Resource Name (ARN) of a change set for which
    -- CloudFormation returns the associated template. If you specify a name,
    -- you must also specify the @StackName@.
    changeSetName :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique stack ID that\'s associated with the stack, which
    -- aren\'t always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | For templates that include transforms, the stage of the template that
    -- CloudFormation returns. To get the user-submitted template, specify
    -- @Original@. To get the template after CloudFormation has processed all
    -- transforms, specify @Processed@.
    --
    -- If the template doesn\'t include transforms, @Original@ and @Processed@
    -- return the same template. By default, CloudFormation specifies
    -- @Processed@.
    templateStage :: Prelude.Maybe TemplateStage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSetName', 'getTemplate_changeSetName' - The name or Amazon Resource Name (ARN) of a change set for which
-- CloudFormation returns the associated template. If you specify a name,
-- you must also specify the @StackName@.
--
-- 'stackName', 'getTemplate_stackName' - The name or the unique stack ID that\'s associated with the stack, which
-- aren\'t always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
--
-- 'templateStage', 'getTemplate_templateStage' - For templates that include transforms, the stage of the template that
-- CloudFormation returns. To get the user-submitted template, specify
-- @Original@. To get the template after CloudFormation has processed all
-- transforms, specify @Processed@.
--
-- If the template doesn\'t include transforms, @Original@ and @Processed@
-- return the same template. By default, CloudFormation specifies
-- @Processed@.
newGetTemplate ::
  GetTemplate
newGetTemplate =
  GetTemplate'
    { changeSetName = Prelude.Nothing,
      stackName = Prelude.Nothing,
      templateStage = Prelude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of a change set for which
-- CloudFormation returns the associated template. If you specify a name,
-- you must also specify the @StackName@.
getTemplate_changeSetName :: Lens.Lens' GetTemplate (Prelude.Maybe Prelude.Text)
getTemplate_changeSetName = Lens.lens (\GetTemplate' {changeSetName} -> changeSetName) (\s@GetTemplate' {} a -> s {changeSetName = a} :: GetTemplate)

-- | The name or the unique stack ID that\'s associated with the stack, which
-- aren\'t always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
getTemplate_stackName :: Lens.Lens' GetTemplate (Prelude.Maybe Prelude.Text)
getTemplate_stackName = Lens.lens (\GetTemplate' {stackName} -> stackName) (\s@GetTemplate' {} a -> s {stackName = a} :: GetTemplate)

-- | For templates that include transforms, the stage of the template that
-- CloudFormation returns. To get the user-submitted template, specify
-- @Original@. To get the template after CloudFormation has processed all
-- transforms, specify @Processed@.
--
-- If the template doesn\'t include transforms, @Original@ and @Processed@
-- return the same template. By default, CloudFormation specifies
-- @Processed@.
getTemplate_templateStage :: Lens.Lens' GetTemplate (Prelude.Maybe TemplateStage)
getTemplate_templateStage = Lens.lens (\GetTemplate' {templateStage} -> templateStage) (\s@GetTemplate' {} a -> s {templateStage = a} :: GetTemplate)

instance Core.AWSRequest GetTemplate where
  type AWSResponse GetTemplate = GetTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Prelude.<$> ( x
                            Data..@? "StagesAvailable"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "TemplateBody")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplate where
  hashWithSalt _salt GetTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` changeSetName
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` templateStage

instance Prelude.NFData GetTemplate where
  rnf GetTemplate' {..} =
    Prelude.rnf changeSetName
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf templateStage

instance Data.ToHeaders GetTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "ChangeSetName" Data.=: changeSetName,
        "StackName" Data.=: stackName,
        "TemplateStage" Data.=: templateStage
      ]

-- | The output for GetTemplate action.
--
-- /See:/ 'newGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { -- | The stage of the template that you can retrieve. For stacks, the
    -- @Original@ and @Processed@ templates are always available. For change
    -- sets, the @Original@ template is always available. After CloudFormation
    -- finishes creating the change set, the @Processed@ template becomes
    -- available.
    stagesAvailable :: Prelude.Maybe [TemplateStage],
    -- | Structure containing the template body. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.)
    --
    -- CloudFormation returns the same template that was used when the stack
    -- was created.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- sets, the @Original@ template is always available. After CloudFormation
-- finishes creating the change set, the @Processed@ template becomes
-- available.
--
-- 'templateBody', 'getTemplateResponse_templateBody' - Structure containing the template body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.)
--
-- CloudFormation returns the same template that was used when the stack
-- was created.
--
-- 'httpStatus', 'getTemplateResponse_httpStatus' - The response's http status code.
newGetTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemplateResponse
newGetTemplateResponse pHttpStatus_ =
  GetTemplateResponse'
    { stagesAvailable =
        Prelude.Nothing,
      templateBody = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stage of the template that you can retrieve. For stacks, the
-- @Original@ and @Processed@ templates are always available. For change
-- sets, the @Original@ template is always available. After CloudFormation
-- finishes creating the change set, the @Processed@ template becomes
-- available.
getTemplateResponse_stagesAvailable :: Lens.Lens' GetTemplateResponse (Prelude.Maybe [TemplateStage])
getTemplateResponse_stagesAvailable = Lens.lens (\GetTemplateResponse' {stagesAvailable} -> stagesAvailable) (\s@GetTemplateResponse' {} a -> s {stagesAvailable = a} :: GetTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | Structure containing the template body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.)
--
-- CloudFormation returns the same template that was used when the stack
-- was created.
getTemplateResponse_templateBody :: Lens.Lens' GetTemplateResponse (Prelude.Maybe Prelude.Text)
getTemplateResponse_templateBody = Lens.lens (\GetTemplateResponse' {templateBody} -> templateBody) (\s@GetTemplateResponse' {} a -> s {templateBody = a} :: GetTemplateResponse)

-- | The response's http status code.
getTemplateResponse_httpStatus :: Lens.Lens' GetTemplateResponse Prelude.Int
getTemplateResponse_httpStatus = Lens.lens (\GetTemplateResponse' {httpStatus} -> httpStatus) (\s@GetTemplateResponse' {} a -> s {httpStatus = a} :: GetTemplateResponse)

instance Prelude.NFData GetTemplateResponse where
  rnf GetTemplateResponse' {..} =
    Prelude.rnf stagesAvailable
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf httpStatus
