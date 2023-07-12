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
-- Module      : Amazonka.OAM.GetLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns complete information about one link.
--
-- To use this operation, provide the link ARN. To retrieve a list of link
-- ARNs, use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListLinks.html ListLinks>.
module Amazonka.OAM.GetLink
  ( -- * Creating a Request
    GetLink (..),
    newGetLink,

    -- * Request Lenses
    getLink_identifier,

    -- * Destructuring the Response
    GetLinkResponse (..),
    newGetLinkResponse,

    -- * Response Lenses
    getLinkResponse_arn,
    getLinkResponse_id,
    getLinkResponse_label,
    getLinkResponse_labelTemplate,
    getLinkResponse_resourceTypes,
    getLinkResponse_sinkArn,
    getLinkResponse_tags,
    getLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLink' smart constructor.
data GetLink = GetLink'
  { -- | The ARN of the link to retrieve information for.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getLink_identifier' - The ARN of the link to retrieve information for.
newGetLink ::
  -- | 'identifier'
  Prelude.Text ->
  GetLink
newGetLink pIdentifier_ =
  GetLink' {identifier = pIdentifier_}

-- | The ARN of the link to retrieve information for.
getLink_identifier :: Lens.Lens' GetLink Prelude.Text
getLink_identifier = Lens.lens (\GetLink' {identifier} -> identifier) (\s@GetLink' {} a -> s {identifier = a} :: GetLink)

instance Core.AWSRequest GetLink where
  type AWSResponse GetLink = GetLinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLinkResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Label")
            Prelude.<*> (x Data..?> "LabelTemplate")
            Prelude.<*> (x Data..?> "ResourceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SinkArn")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLink where
  hashWithSalt _salt GetLink' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetLink where
  rnf GetLink' {..} = Prelude.rnf identifier

instance Data.ToHeaders GetLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLink where
  toJSON GetLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

instance Data.ToPath GetLink where
  toPath = Prelude.const "/GetLink"

instance Data.ToQuery GetLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLinkResponse' smart constructor.
data GetLinkResponse = GetLinkResponse'
  { -- | The ARN of the link.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- link ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The label that you assigned to this link, with the variables resolved to
    -- their actual values.
    label :: Prelude.Maybe Prelude.Text,
    -- | The exact label template that was specified when the link was created,
    -- with the template variables not resolved.
    labelTemplate :: Prelude.Maybe Prelude.Text,
    -- | The resource types supported by this link.
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
-- Create a value of 'GetLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getLinkResponse_arn' - The ARN of the link.
--
-- 'id', 'getLinkResponse_id' - The random ID string that Amazon Web Services generated as part of the
-- link ARN.
--
-- 'label', 'getLinkResponse_label' - The label that you assigned to this link, with the variables resolved to
-- their actual values.
--
-- 'labelTemplate', 'getLinkResponse_labelTemplate' - The exact label template that was specified when the link was created,
-- with the template variables not resolved.
--
-- 'resourceTypes', 'getLinkResponse_resourceTypes' - The resource types supported by this link.
--
-- 'sinkArn', 'getLinkResponse_sinkArn' - The ARN of the sink that is used for this link.
--
-- 'tags', 'getLinkResponse_tags' - The tags assigned to the link.
--
-- 'httpStatus', 'getLinkResponse_httpStatus' - The response's http status code.
newGetLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLinkResponse
newGetLinkResponse pHttpStatus_ =
  GetLinkResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      label = Prelude.Nothing,
      labelTemplate = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the link.
getLinkResponse_arn :: Lens.Lens' GetLinkResponse (Prelude.Maybe Prelude.Text)
getLinkResponse_arn = Lens.lens (\GetLinkResponse' {arn} -> arn) (\s@GetLinkResponse' {} a -> s {arn = a} :: GetLinkResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- link ARN.
getLinkResponse_id :: Lens.Lens' GetLinkResponse (Prelude.Maybe Prelude.Text)
getLinkResponse_id = Lens.lens (\GetLinkResponse' {id} -> id) (\s@GetLinkResponse' {} a -> s {id = a} :: GetLinkResponse)

-- | The label that you assigned to this link, with the variables resolved to
-- their actual values.
getLinkResponse_label :: Lens.Lens' GetLinkResponse (Prelude.Maybe Prelude.Text)
getLinkResponse_label = Lens.lens (\GetLinkResponse' {label} -> label) (\s@GetLinkResponse' {} a -> s {label = a} :: GetLinkResponse)

-- | The exact label template that was specified when the link was created,
-- with the template variables not resolved.
getLinkResponse_labelTemplate :: Lens.Lens' GetLinkResponse (Prelude.Maybe Prelude.Text)
getLinkResponse_labelTemplate = Lens.lens (\GetLinkResponse' {labelTemplate} -> labelTemplate) (\s@GetLinkResponse' {} a -> s {labelTemplate = a} :: GetLinkResponse)

-- | The resource types supported by this link.
getLinkResponse_resourceTypes :: Lens.Lens' GetLinkResponse (Prelude.Maybe [Prelude.Text])
getLinkResponse_resourceTypes = Lens.lens (\GetLinkResponse' {resourceTypes} -> resourceTypes) (\s@GetLinkResponse' {} a -> s {resourceTypes = a} :: GetLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the sink that is used for this link.
getLinkResponse_sinkArn :: Lens.Lens' GetLinkResponse (Prelude.Maybe Prelude.Text)
getLinkResponse_sinkArn = Lens.lens (\GetLinkResponse' {sinkArn} -> sinkArn) (\s@GetLinkResponse' {} a -> s {sinkArn = a} :: GetLinkResponse)

-- | The tags assigned to the link.
getLinkResponse_tags :: Lens.Lens' GetLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getLinkResponse_tags = Lens.lens (\GetLinkResponse' {tags} -> tags) (\s@GetLinkResponse' {} a -> s {tags = a} :: GetLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLinkResponse_httpStatus :: Lens.Lens' GetLinkResponse Prelude.Int
getLinkResponse_httpStatus = Lens.lens (\GetLinkResponse' {httpStatus} -> httpStatus) (\s@GetLinkResponse' {} a -> s {httpStatus = a} :: GetLinkResponse)

instance Prelude.NFData GetLinkResponse where
  rnf GetLinkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf labelTemplate
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
