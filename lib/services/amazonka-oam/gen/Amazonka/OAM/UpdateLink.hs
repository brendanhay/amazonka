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
-- Module      : Amazonka.OAM.UpdateLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to change what types of data are shared from a source
-- account to its linked monitoring account sink. You can\'t change the
-- sink or change the monitoring account with this operation.
--
-- To update the list of tags associated with the sink, use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_TagResource.html TagResource>.
module Amazonka.OAM.UpdateLink
  ( -- * Creating a Request
    UpdateLink (..),
    newUpdateLink,

    -- * Request Lenses
    updateLink_identifier,
    updateLink_resourceTypes,

    -- * Destructuring the Response
    UpdateLinkResponse (..),
    newUpdateLinkResponse,

    -- * Response Lenses
    updateLinkResponse_arn,
    updateLinkResponse_id,
    updateLinkResponse_label,
    updateLinkResponse_labelTemplate,
    updateLinkResponse_resourceTypes,
    updateLinkResponse_sinkArn,
    updateLinkResponse_tags,
    updateLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLink' smart constructor.
data UpdateLink = UpdateLink'
  { -- | The ARN of the link that you want to update.
    identifier :: Prelude.Text,
    -- | An array of strings that define which types of data that the source
    -- account will send to the monitoring account.
    --
    -- Your input here replaces the current set of data types that are shared.
    resourceTypes :: Prelude.NonEmpty ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'updateLink_identifier' - The ARN of the link that you want to update.
--
-- 'resourceTypes', 'updateLink_resourceTypes' - An array of strings that define which types of data that the source
-- account will send to the monitoring account.
--
-- Your input here replaces the current set of data types that are shared.
newUpdateLink ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'resourceTypes'
  Prelude.NonEmpty ResourceType ->
  UpdateLink
newUpdateLink pIdentifier_ pResourceTypes_ =
  UpdateLink'
    { identifier = pIdentifier_,
      resourceTypes = Lens.coerced Lens.# pResourceTypes_
    }

-- | The ARN of the link that you want to update.
updateLink_identifier :: Lens.Lens' UpdateLink Prelude.Text
updateLink_identifier = Lens.lens (\UpdateLink' {identifier} -> identifier) (\s@UpdateLink' {} a -> s {identifier = a} :: UpdateLink)

-- | An array of strings that define which types of data that the source
-- account will send to the monitoring account.
--
-- Your input here replaces the current set of data types that are shared.
updateLink_resourceTypes :: Lens.Lens' UpdateLink (Prelude.NonEmpty ResourceType)
updateLink_resourceTypes = Lens.lens (\UpdateLink' {resourceTypes} -> resourceTypes) (\s@UpdateLink' {} a -> s {resourceTypes = a} :: UpdateLink) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateLink where
  type AWSResponse UpdateLink = UpdateLinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLinkResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Label")
            Prelude.<*> (x Data..?> "LabelTemplate")
            Prelude.<*> (x Data..?> "ResourceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SinkArn")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLink where
  hashWithSalt _salt UpdateLink' {..} =
    _salt `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData UpdateLink where
  rnf UpdateLink' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf resourceTypes

instance Data.ToHeaders UpdateLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLink where
  toJSON UpdateLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Identifier" Data..= identifier),
            Prelude.Just
              ("ResourceTypes" Data..= resourceTypes)
          ]
      )

instance Data.ToPath UpdateLink where
  toPath = Prelude.const "/UpdateLink"

instance Data.ToQuery UpdateLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLinkResponse' smart constructor.
data UpdateLinkResponse = UpdateLinkResponse'
  { -- | The ARN of the link that you have updated.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- sink ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The label assigned to this link, with the variables resolved to their
    -- actual values.
    label :: Prelude.Maybe Prelude.Text,
    -- | The exact label template that was specified when the link was created,
    -- with the template variables not resolved.
    labelTemplate :: Prelude.Maybe Prelude.Text,
    -- | The resource types now supported by this link.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the sink that is used for this link.
    sinkArn :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the link.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateLinkResponse_arn' - The ARN of the link that you have updated.
--
-- 'id', 'updateLinkResponse_id' - The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
--
-- 'label', 'updateLinkResponse_label' - The label assigned to this link, with the variables resolved to their
-- actual values.
--
-- 'labelTemplate', 'updateLinkResponse_labelTemplate' - The exact label template that was specified when the link was created,
-- with the template variables not resolved.
--
-- 'resourceTypes', 'updateLinkResponse_resourceTypes' - The resource types now supported by this link.
--
-- 'sinkArn', 'updateLinkResponse_sinkArn' - The ARN of the sink that is used for this link.
--
-- 'tags', 'updateLinkResponse_tags' - The tags assigned to the link.
--
-- 'httpStatus', 'updateLinkResponse_httpStatus' - The response's http status code.
newUpdateLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLinkResponse
newUpdateLinkResponse pHttpStatus_ =
  UpdateLinkResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      label = Prelude.Nothing,
      labelTemplate = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the link that you have updated.
updateLinkResponse_arn :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe Prelude.Text)
updateLinkResponse_arn = Lens.lens (\UpdateLinkResponse' {arn} -> arn) (\s@UpdateLinkResponse' {} a -> s {arn = a} :: UpdateLinkResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
updateLinkResponse_id :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe Prelude.Text)
updateLinkResponse_id = Lens.lens (\UpdateLinkResponse' {id} -> id) (\s@UpdateLinkResponse' {} a -> s {id = a} :: UpdateLinkResponse)

-- | The label assigned to this link, with the variables resolved to their
-- actual values.
updateLinkResponse_label :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe Prelude.Text)
updateLinkResponse_label = Lens.lens (\UpdateLinkResponse' {label} -> label) (\s@UpdateLinkResponse' {} a -> s {label = a} :: UpdateLinkResponse)

-- | The exact label template that was specified when the link was created,
-- with the template variables not resolved.
updateLinkResponse_labelTemplate :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe Prelude.Text)
updateLinkResponse_labelTemplate = Lens.lens (\UpdateLinkResponse' {labelTemplate} -> labelTemplate) (\s@UpdateLinkResponse' {} a -> s {labelTemplate = a} :: UpdateLinkResponse)

-- | The resource types now supported by this link.
updateLinkResponse_resourceTypes :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe [Prelude.Text])
updateLinkResponse_resourceTypes = Lens.lens (\UpdateLinkResponse' {resourceTypes} -> resourceTypes) (\s@UpdateLinkResponse' {} a -> s {resourceTypes = a} :: UpdateLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the sink that is used for this link.
updateLinkResponse_sinkArn :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe Prelude.Text)
updateLinkResponse_sinkArn = Lens.lens (\UpdateLinkResponse' {sinkArn} -> sinkArn) (\s@UpdateLinkResponse' {} a -> s {sinkArn = a} :: UpdateLinkResponse)

-- | The tags assigned to the link.
updateLinkResponse_tags :: Lens.Lens' UpdateLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateLinkResponse_tags = Lens.lens (\UpdateLinkResponse' {tags} -> tags) (\s@UpdateLinkResponse' {} a -> s {tags = a} :: UpdateLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateLinkResponse_httpStatus :: Lens.Lens' UpdateLinkResponse Prelude.Int
updateLinkResponse_httpStatus = Lens.lens (\UpdateLinkResponse' {httpStatus} -> httpStatus) (\s@UpdateLinkResponse' {} a -> s {httpStatus = a} :: UpdateLinkResponse)

instance Prelude.NFData UpdateLinkResponse where
  rnf UpdateLinkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf labelTemplate
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
