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
-- Module      : Amazonka.SageMaker.CreateHumanTaskUi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines the settings you will use for the human review workflow user
-- interface. Reviewers will see a three-panel interface with an
-- instruction area, the item to review, and an input area.
module Amazonka.SageMaker.CreateHumanTaskUi
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateHumanTaskUi' smart constructor.
data CreateHumanTaskUi = CreateHumanTaskUi'
  { -- | An array of key-value pairs that contain metadata to help you categorize
    -- and organize a human review workflow user interface. Each tag consists
    -- of a key and a value, both of which you define.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the user interface you are creating.
    humanTaskUiName :: Prelude.Text,
    uiTemplate :: UiTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'uiTemplate'
  UiTemplate ->
  CreateHumanTaskUi
newCreateHumanTaskUi pHumanTaskUiName_ pUiTemplate_ =
  CreateHumanTaskUi'
    { tags = Prelude.Nothing,
      humanTaskUiName = pHumanTaskUiName_,
      uiTemplate = pUiTemplate_
    }

-- | An array of key-value pairs that contain metadata to help you categorize
-- and organize a human review workflow user interface. Each tag consists
-- of a key and a value, both of which you define.
createHumanTaskUi_tags :: Lens.Lens' CreateHumanTaskUi (Prelude.Maybe [Tag])
createHumanTaskUi_tags = Lens.lens (\CreateHumanTaskUi' {tags} -> tags) (\s@CreateHumanTaskUi' {} a -> s {tags = a} :: CreateHumanTaskUi) Prelude.. Lens.mapping Lens.coerced

-- | The name of the user interface you are creating.
createHumanTaskUi_humanTaskUiName :: Lens.Lens' CreateHumanTaskUi Prelude.Text
createHumanTaskUi_humanTaskUiName = Lens.lens (\CreateHumanTaskUi' {humanTaskUiName} -> humanTaskUiName) (\s@CreateHumanTaskUi' {} a -> s {humanTaskUiName = a} :: CreateHumanTaskUi)

-- | Undocumented member.
createHumanTaskUi_uiTemplate :: Lens.Lens' CreateHumanTaskUi UiTemplate
createHumanTaskUi_uiTemplate = Lens.lens (\CreateHumanTaskUi' {uiTemplate} -> uiTemplate) (\s@CreateHumanTaskUi' {} a -> s {uiTemplate = a} :: CreateHumanTaskUi)

instance Core.AWSRequest CreateHumanTaskUi where
  type
    AWSResponse CreateHumanTaskUi =
      CreateHumanTaskUiResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHumanTaskUiResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HumanTaskUiArn")
      )

instance Prelude.Hashable CreateHumanTaskUi where
  hashWithSalt _salt CreateHumanTaskUi' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` humanTaskUiName
      `Prelude.hashWithSalt` uiTemplate

instance Prelude.NFData CreateHumanTaskUi where
  rnf CreateHumanTaskUi' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf humanTaskUiName
      `Prelude.seq` Prelude.rnf uiTemplate

instance Data.ToHeaders CreateHumanTaskUi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateHumanTaskUi" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHumanTaskUi where
  toJSON CreateHumanTaskUi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("HumanTaskUiName" Data..= humanTaskUiName),
            Prelude.Just ("UiTemplate" Data..= uiTemplate)
          ]
      )

instance Data.ToPath CreateHumanTaskUi where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHumanTaskUi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHumanTaskUiResponse' smart constructor.
data CreateHumanTaskUiResponse = CreateHumanTaskUiResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the human review workflow user
    -- interface you create.
    humanTaskUiArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'humanTaskUiArn'
  Prelude.Text ->
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
createHumanTaskUiResponse_httpStatus :: Lens.Lens' CreateHumanTaskUiResponse Prelude.Int
createHumanTaskUiResponse_httpStatus = Lens.lens (\CreateHumanTaskUiResponse' {httpStatus} -> httpStatus) (\s@CreateHumanTaskUiResponse' {} a -> s {httpStatus = a} :: CreateHumanTaskUiResponse)

-- | The Amazon Resource Name (ARN) of the human review workflow user
-- interface you create.
createHumanTaskUiResponse_humanTaskUiArn :: Lens.Lens' CreateHumanTaskUiResponse Prelude.Text
createHumanTaskUiResponse_humanTaskUiArn = Lens.lens (\CreateHumanTaskUiResponse' {humanTaskUiArn} -> humanTaskUiArn) (\s@CreateHumanTaskUiResponse' {} a -> s {humanTaskUiArn = a} :: CreateHumanTaskUiResponse)

instance Prelude.NFData CreateHumanTaskUiResponse where
  rnf CreateHumanTaskUiResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf humanTaskUiArn
