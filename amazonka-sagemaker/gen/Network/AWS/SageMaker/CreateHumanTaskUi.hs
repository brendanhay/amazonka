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
-- Module      : Network.AWS.SageMaker.CreateHumanTaskUi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the settings you will use for the human review workflow user
-- interface. Reviewers will see a three-panel interface with an
-- instruction area, the item to review, and an input area.
module Network.AWS.SageMaker.CreateHumanTaskUi
  ( -- * Creating a Request
    CreateHumanTaskUi (..),
    newCreateHumanTaskUi,

    -- * Request Lenses
    createHumanTaskUi_tags,
    createHumanTaskUi_humanTaskUiName,
    createHumanTaskUi_uiTemplate,

    -- * Destructuring the Response
    CreateHumanTaskUiResponse (..),
    newCreateHumanTaskUiResponse,

    -- * Response Lenses
    createHumanTaskUiResponse_httpStatus,
    createHumanTaskUiResponse_humanTaskUiArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateHumanTaskUi' smart constructor.
data CreateHumanTaskUi = CreateHumanTaskUi'
  { -- | An array of key-value pairs that contain metadata to help you categorize
    -- and organize a human review workflow user interface. Each tag consists
    -- of a key and a value, both of which you define.
    tags :: Core.Maybe [Tag],
    -- | The name of the user interface you are creating.
    humanTaskUiName :: Core.Text,
    uiTemplate :: UiTemplate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHumanTaskUi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createHumanTaskUi_tags' - An array of key-value pairs that contain metadata to help you categorize
-- and organize a human review workflow user interface. Each tag consists
-- of a key and a value, both of which you define.
--
-- 'humanTaskUiName', 'createHumanTaskUi_humanTaskUiName' - The name of the user interface you are creating.
--
-- 'uiTemplate', 'createHumanTaskUi_uiTemplate' - Undocumented member.
newCreateHumanTaskUi ::
  -- | 'humanTaskUiName'
  Core.Text ->
  -- | 'uiTemplate'
  UiTemplate ->
  CreateHumanTaskUi
newCreateHumanTaskUi pHumanTaskUiName_ pUiTemplate_ =
  CreateHumanTaskUi'
    { tags = Core.Nothing,
      humanTaskUiName = pHumanTaskUiName_,
      uiTemplate = pUiTemplate_
    }

-- | An array of key-value pairs that contain metadata to help you categorize
-- and organize a human review workflow user interface. Each tag consists
-- of a key and a value, both of which you define.
createHumanTaskUi_tags :: Lens.Lens' CreateHumanTaskUi (Core.Maybe [Tag])
createHumanTaskUi_tags = Lens.lens (\CreateHumanTaskUi' {tags} -> tags) (\s@CreateHumanTaskUi' {} a -> s {tags = a} :: CreateHumanTaskUi) Core.. Lens.mapping Lens._Coerce

-- | The name of the user interface you are creating.
createHumanTaskUi_humanTaskUiName :: Lens.Lens' CreateHumanTaskUi Core.Text
createHumanTaskUi_humanTaskUiName = Lens.lens (\CreateHumanTaskUi' {humanTaskUiName} -> humanTaskUiName) (\s@CreateHumanTaskUi' {} a -> s {humanTaskUiName = a} :: CreateHumanTaskUi)

-- | Undocumented member.
createHumanTaskUi_uiTemplate :: Lens.Lens' CreateHumanTaskUi UiTemplate
createHumanTaskUi_uiTemplate = Lens.lens (\CreateHumanTaskUi' {uiTemplate} -> uiTemplate) (\s@CreateHumanTaskUi' {} a -> s {uiTemplate = a} :: CreateHumanTaskUi)

instance Core.AWSRequest CreateHumanTaskUi where
  type
    AWSResponse CreateHumanTaskUi =
      CreateHumanTaskUiResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHumanTaskUiResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "HumanTaskUiArn")
      )

instance Core.Hashable CreateHumanTaskUi

instance Core.NFData CreateHumanTaskUi

instance Core.ToHeaders CreateHumanTaskUi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateHumanTaskUi" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateHumanTaskUi where
  toJSON CreateHumanTaskUi' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just
              ("HumanTaskUiName" Core..= humanTaskUiName),
            Core.Just ("UiTemplate" Core..= uiTemplate)
          ]
      )

instance Core.ToPath CreateHumanTaskUi where
  toPath = Core.const "/"

instance Core.ToQuery CreateHumanTaskUi where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateHumanTaskUiResponse' smart constructor.
data CreateHumanTaskUiResponse = CreateHumanTaskUiResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the human review workflow user
    -- interface you create.
    humanTaskUiArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHumanTaskUiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createHumanTaskUiResponse_httpStatus' - The response's http status code.
--
-- 'humanTaskUiArn', 'createHumanTaskUiResponse_humanTaskUiArn' - The Amazon Resource Name (ARN) of the human review workflow user
-- interface you create.
newCreateHumanTaskUiResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'humanTaskUiArn'
  Core.Text ->
  CreateHumanTaskUiResponse
newCreateHumanTaskUiResponse
  pHttpStatus_
  pHumanTaskUiArn_ =
    CreateHumanTaskUiResponse'
      { httpStatus =
          pHttpStatus_,
        humanTaskUiArn = pHumanTaskUiArn_
      }

-- | The response's http status code.
createHumanTaskUiResponse_httpStatus :: Lens.Lens' CreateHumanTaskUiResponse Core.Int
createHumanTaskUiResponse_httpStatus = Lens.lens (\CreateHumanTaskUiResponse' {httpStatus} -> httpStatus) (\s@CreateHumanTaskUiResponse' {} a -> s {httpStatus = a} :: CreateHumanTaskUiResponse)

-- | The Amazon Resource Name (ARN) of the human review workflow user
-- interface you create.
createHumanTaskUiResponse_humanTaskUiArn :: Lens.Lens' CreateHumanTaskUiResponse Core.Text
createHumanTaskUiResponse_humanTaskUiArn = Lens.lens (\CreateHumanTaskUiResponse' {humanTaskUiArn} -> humanTaskUiArn) (\s@CreateHumanTaskUiResponse' {} a -> s {humanTaskUiArn = a} :: CreateHumanTaskUiResponse)

instance Core.NFData CreateHumanTaskUiResponse
