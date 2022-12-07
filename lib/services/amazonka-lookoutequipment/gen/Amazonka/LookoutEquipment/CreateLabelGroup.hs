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
-- Module      : Amazonka.LookoutEquipment.CreateLabelGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group of labels.
module Amazonka.LookoutEquipment.CreateLabelGroup
  ( -- * Creating a Request
    CreateLabelGroup (..),
    newCreateLabelGroup,

    -- * Request Lenses
    createLabelGroup_tags,
    createLabelGroup_faultCodes,
    createLabelGroup_labelGroupName,
    createLabelGroup_clientToken,

    -- * Destructuring the Response
    CreateLabelGroupResponse (..),
    newCreateLabelGroupResponse,

    -- * Response Lenses
    createLabelGroupResponse_labelGroupArn,
    createLabelGroupResponse_labelGroupName,
    createLabelGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLabelGroup' smart constructor.
data CreateLabelGroup = CreateLabelGroup'
  { -- | Tags that provide metadata about the label group you are creating.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    tags :: Prelude.Maybe [Tag],
    -- | The acceptable fault codes (indicating the type of anomaly associated
    -- with the label) that can be used with this label group.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    faultCodes :: Prelude.Maybe [Prelude.Text],
    -- | Names a group of labels.
    --
    -- Data in this field will be retained for service usage. Follow best
    -- practices for the security of your data.
    labelGroupName :: Prelude.Text,
    -- | A unique identifier for the request to create a label group. If you do
    -- not set the client request token, Lookout for Equipment generates one.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLabelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLabelGroup_tags' - Tags that provide metadata about the label group you are creating.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'faultCodes', 'createLabelGroup_faultCodes' - The acceptable fault codes (indicating the type of anomaly associated
-- with the label) that can be used with this label group.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'labelGroupName', 'createLabelGroup_labelGroupName' - Names a group of labels.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
--
-- 'clientToken', 'createLabelGroup_clientToken' - A unique identifier for the request to create a label group. If you do
-- not set the client request token, Lookout for Equipment generates one.
newCreateLabelGroup ::
  -- | 'labelGroupName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateLabelGroup
newCreateLabelGroup pLabelGroupName_ pClientToken_ =
  CreateLabelGroup'
    { tags = Prelude.Nothing,
      faultCodes = Prelude.Nothing,
      labelGroupName = pLabelGroupName_,
      clientToken = pClientToken_
    }

-- | Tags that provide metadata about the label group you are creating.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabelGroup_tags :: Lens.Lens' CreateLabelGroup (Prelude.Maybe [Tag])
createLabelGroup_tags = Lens.lens (\CreateLabelGroup' {tags} -> tags) (\s@CreateLabelGroup' {} a -> s {tags = a} :: CreateLabelGroup) Prelude.. Lens.mapping Lens.coerced

-- | The acceptable fault codes (indicating the type of anomaly associated
-- with the label) that can be used with this label group.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabelGroup_faultCodes :: Lens.Lens' CreateLabelGroup (Prelude.Maybe [Prelude.Text])
createLabelGroup_faultCodes = Lens.lens (\CreateLabelGroup' {faultCodes} -> faultCodes) (\s@CreateLabelGroup' {} a -> s {faultCodes = a} :: CreateLabelGroup) Prelude.. Lens.mapping Lens.coerced

-- | Names a group of labels.
--
-- Data in this field will be retained for service usage. Follow best
-- practices for the security of your data.
createLabelGroup_labelGroupName :: Lens.Lens' CreateLabelGroup Prelude.Text
createLabelGroup_labelGroupName = Lens.lens (\CreateLabelGroup' {labelGroupName} -> labelGroupName) (\s@CreateLabelGroup' {} a -> s {labelGroupName = a} :: CreateLabelGroup)

-- | A unique identifier for the request to create a label group. If you do
-- not set the client request token, Lookout for Equipment generates one.
createLabelGroup_clientToken :: Lens.Lens' CreateLabelGroup Prelude.Text
createLabelGroup_clientToken = Lens.lens (\CreateLabelGroup' {clientToken} -> clientToken) (\s@CreateLabelGroup' {} a -> s {clientToken = a} :: CreateLabelGroup)

instance Core.AWSRequest CreateLabelGroup where
  type
    AWSResponse CreateLabelGroup =
      CreateLabelGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLabelGroupResponse'
            Prelude.<$> (x Data..?> "LabelGroupArn")
            Prelude.<*> (x Data..?> "LabelGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLabelGroup where
  hashWithSalt _salt CreateLabelGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` faultCodes
      `Prelude.hashWithSalt` labelGroupName
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateLabelGroup where
  rnf CreateLabelGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf faultCodes
      `Prelude.seq` Prelude.rnf labelGroupName
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateLabelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.CreateLabelGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLabelGroup where
  toJSON CreateLabelGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("FaultCodes" Data..=) Prelude.<$> faultCodes,
            Prelude.Just
              ("LabelGroupName" Data..= labelGroupName),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateLabelGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLabelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLabelGroupResponse' smart constructor.
data CreateLabelGroupResponse = CreateLabelGroupResponse'
  { -- | The ARN of the label group that you have created.
    labelGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the label group that you have created. Data in this field
    -- will be retained for service usage. Follow best practices for the
    -- security of your data.
    labelGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLabelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupArn', 'createLabelGroupResponse_labelGroupArn' - The ARN of the label group that you have created.
--
-- 'labelGroupName', 'createLabelGroupResponse_labelGroupName' - The name of the label group that you have created. Data in this field
-- will be retained for service usage. Follow best practices for the
-- security of your data.
--
-- 'httpStatus', 'createLabelGroupResponse_httpStatus' - The response's http status code.
newCreateLabelGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLabelGroupResponse
newCreateLabelGroupResponse pHttpStatus_ =
  CreateLabelGroupResponse'
    { labelGroupArn =
        Prelude.Nothing,
      labelGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the label group that you have created.
createLabelGroupResponse_labelGroupArn :: Lens.Lens' CreateLabelGroupResponse (Prelude.Maybe Prelude.Text)
createLabelGroupResponse_labelGroupArn = Lens.lens (\CreateLabelGroupResponse' {labelGroupArn} -> labelGroupArn) (\s@CreateLabelGroupResponse' {} a -> s {labelGroupArn = a} :: CreateLabelGroupResponse)

-- | The name of the label group that you have created. Data in this field
-- will be retained for service usage. Follow best practices for the
-- security of your data.
createLabelGroupResponse_labelGroupName :: Lens.Lens' CreateLabelGroupResponse (Prelude.Maybe Prelude.Text)
createLabelGroupResponse_labelGroupName = Lens.lens (\CreateLabelGroupResponse' {labelGroupName} -> labelGroupName) (\s@CreateLabelGroupResponse' {} a -> s {labelGroupName = a} :: CreateLabelGroupResponse)

-- | The response's http status code.
createLabelGroupResponse_httpStatus :: Lens.Lens' CreateLabelGroupResponse Prelude.Int
createLabelGroupResponse_httpStatus = Lens.lens (\CreateLabelGroupResponse' {httpStatus} -> httpStatus) (\s@CreateLabelGroupResponse' {} a -> s {httpStatus = a} :: CreateLabelGroupResponse)

instance Prelude.NFData CreateLabelGroupResponse where
  rnf CreateLabelGroupResponse' {..} =
    Prelude.rnf labelGroupArn
      `Prelude.seq` Prelude.rnf labelGroupName
      `Prelude.seq` Prelude.rnf httpStatus
