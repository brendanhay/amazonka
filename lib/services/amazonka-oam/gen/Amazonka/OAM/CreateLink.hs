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
-- Module      : Amazonka.OAM.CreateLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a link between a source account and a sink that you have created
-- in a monitoring account.
--
-- Before you create a link, you must create a sink in the monitoring
-- account and create a sink policy in that account. The sink policy must
-- permit the source account to link to it. You can grant permission to
-- source accounts by granting permission to an entire organization or to
-- individual accounts.
--
-- For more information, see
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_CreateSink.html CreateSink>
-- and
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_PutSinkPolicy.html PutSinkPolicy>.
--
-- Each monitoring account can be linked to as many as 100,000 source
-- accounts.
--
-- Each source account can be linked to as many as five monitoring
-- accounts.
module Amazonka.OAM.CreateLink
  ( -- * Creating a Request
    CreateLink (..),
    newCreateLink,

    -- * Request Lenses
    createLink_tags,
    createLink_labelTemplate,
    createLink_resourceTypes,
    createLink_sinkIdentifier,

    -- * Destructuring the Response
    CreateLinkResponse (..),
    newCreateLinkResponse,

    -- * Response Lenses
    createLinkResponse_arn,
    createLinkResponse_id,
    createLinkResponse_label,
    createLinkResponse_labelTemplate,
    createLinkResponse_resourceTypes,
    createLinkResponse_sinkArn,
    createLinkResponse_tags,
    createLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLink' smart constructor.
data CreateLink = CreateLink'
  { -- | Assigns one or more tags (key-value pairs) to the link.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- For more information about using tags to control access, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specify a friendly human-readable name to use to identify this source
    -- account when you are viewing data from it in the monitoring account.
    --
    -- You can use a custom label or use the following variables:
    --
    -- -   @$AccountName@ is the name of the account
    --
    -- -   @$AccountEmail@ is the globally unique email address of the account
    --
    -- -   @$AccountEmailNoDomain@ is the email address of the account without
    --     the domain name
    labelTemplate :: Prelude.Text,
    -- | An array of strings that define which types of data that the source
    -- account shares with the monitoring account.
    resourceTypes :: Prelude.NonEmpty ResourceType,
    -- | The ARN of the sink to use to create this link. You can use
    -- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListSinks.html ListSinks>
    -- to find the ARNs of sinks.
    --
    -- For more information about sinks, see
    -- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_CreateSink.html CreateSink>.
    sinkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLink_tags' - Assigns one or more tags (key-value pairs) to the link.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- For more information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
--
-- 'labelTemplate', 'createLink_labelTemplate' - Specify a friendly human-readable name to use to identify this source
-- account when you are viewing data from it in the monitoring account.
--
-- You can use a custom label or use the following variables:
--
-- -   @$AccountName@ is the name of the account
--
-- -   @$AccountEmail@ is the globally unique email address of the account
--
-- -   @$AccountEmailNoDomain@ is the email address of the account without
--     the domain name
--
-- 'resourceTypes', 'createLink_resourceTypes' - An array of strings that define which types of data that the source
-- account shares with the monitoring account.
--
-- 'sinkIdentifier', 'createLink_sinkIdentifier' - The ARN of the sink to use to create this link. You can use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListSinks.html ListSinks>
-- to find the ARNs of sinks.
--
-- For more information about sinks, see
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_CreateSink.html CreateSink>.
newCreateLink ::
  -- | 'labelTemplate'
  Prelude.Text ->
  -- | 'resourceTypes'
  Prelude.NonEmpty ResourceType ->
  -- | 'sinkIdentifier'
  Prelude.Text ->
  CreateLink
newCreateLink
  pLabelTemplate_
  pResourceTypes_
  pSinkIdentifier_ =
    CreateLink'
      { tags = Prelude.Nothing,
        labelTemplate = pLabelTemplate_,
        resourceTypes = Lens.coerced Lens.# pResourceTypes_,
        sinkIdentifier = pSinkIdentifier_
      }

-- | Assigns one or more tags (key-value pairs) to the link.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- For more information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
createLink_tags :: Lens.Lens' CreateLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLink_tags = Lens.lens (\CreateLink' {tags} -> tags) (\s@CreateLink' {} a -> s {tags = a} :: CreateLink) Prelude.. Lens.mapping Lens.coerced

-- | Specify a friendly human-readable name to use to identify this source
-- account when you are viewing data from it in the monitoring account.
--
-- You can use a custom label or use the following variables:
--
-- -   @$AccountName@ is the name of the account
--
-- -   @$AccountEmail@ is the globally unique email address of the account
--
-- -   @$AccountEmailNoDomain@ is the email address of the account without
--     the domain name
createLink_labelTemplate :: Lens.Lens' CreateLink Prelude.Text
createLink_labelTemplate = Lens.lens (\CreateLink' {labelTemplate} -> labelTemplate) (\s@CreateLink' {} a -> s {labelTemplate = a} :: CreateLink)

-- | An array of strings that define which types of data that the source
-- account shares with the monitoring account.
createLink_resourceTypes :: Lens.Lens' CreateLink (Prelude.NonEmpty ResourceType)
createLink_resourceTypes = Lens.lens (\CreateLink' {resourceTypes} -> resourceTypes) (\s@CreateLink' {} a -> s {resourceTypes = a} :: CreateLink) Prelude.. Lens.coerced

-- | The ARN of the sink to use to create this link. You can use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListSinks.html ListSinks>
-- to find the ARNs of sinks.
--
-- For more information about sinks, see
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_CreateSink.html CreateSink>.
createLink_sinkIdentifier :: Lens.Lens' CreateLink Prelude.Text
createLink_sinkIdentifier = Lens.lens (\CreateLink' {sinkIdentifier} -> sinkIdentifier) (\s@CreateLink' {} a -> s {sinkIdentifier = a} :: CreateLink)

instance Core.AWSRequest CreateLink where
  type AWSResponse CreateLink = CreateLinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLinkResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Label")
            Prelude.<*> (x Data..?> "LabelTemplate")
            Prelude.<*> (x Data..?> "ResourceTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SinkArn")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLink where
  hashWithSalt _salt CreateLink' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` labelTemplate
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` sinkIdentifier

instance Prelude.NFData CreateLink where
  rnf CreateLink' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf labelTemplate
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf sinkIdentifier

instance Data.ToHeaders CreateLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLink where
  toJSON CreateLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("LabelTemplate" Data..= labelTemplate),
            Prelude.Just ("ResourceTypes" Data..= resourceTypes),
            Prelude.Just
              ("SinkIdentifier" Data..= sinkIdentifier)
          ]
      )

instance Data.ToPath CreateLink where
  toPath = Prelude.const "/CreateLink"

instance Data.ToQuery CreateLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLinkResponse' smart constructor.
data CreateLinkResponse = CreateLinkResponse'
  { -- | The ARN of the link that is newly created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- link ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The label that you assigned to this link. If the @labelTemplate@
    -- includes variables, this field displays the variables resolved to their
    -- actual values.
    label :: Prelude.Maybe Prelude.Text,
    -- | The exact label template that you specified, with the variables not
    -- resolved.
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
-- Create a value of 'CreateLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createLinkResponse_arn' - The ARN of the link that is newly created.
--
-- 'id', 'createLinkResponse_id' - The random ID string that Amazon Web Services generated as part of the
-- link ARN.
--
-- 'label', 'createLinkResponse_label' - The label that you assigned to this link. If the @labelTemplate@
-- includes variables, this field displays the variables resolved to their
-- actual values.
--
-- 'labelTemplate', 'createLinkResponse_labelTemplate' - The exact label template that you specified, with the variables not
-- resolved.
--
-- 'resourceTypes', 'createLinkResponse_resourceTypes' - The resource types supported by this link.
--
-- 'sinkArn', 'createLinkResponse_sinkArn' - The ARN of the sink that is used for this link.
--
-- 'tags', 'createLinkResponse_tags' - The tags assigned to the link.
--
-- 'httpStatus', 'createLinkResponse_httpStatus' - The response's http status code.
newCreateLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLinkResponse
newCreateLinkResponse pHttpStatus_ =
  CreateLinkResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      label = Prelude.Nothing,
      labelTemplate = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the link that is newly created.
createLinkResponse_arn :: Lens.Lens' CreateLinkResponse (Prelude.Maybe Prelude.Text)
createLinkResponse_arn = Lens.lens (\CreateLinkResponse' {arn} -> arn) (\s@CreateLinkResponse' {} a -> s {arn = a} :: CreateLinkResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- link ARN.
createLinkResponse_id :: Lens.Lens' CreateLinkResponse (Prelude.Maybe Prelude.Text)
createLinkResponse_id = Lens.lens (\CreateLinkResponse' {id} -> id) (\s@CreateLinkResponse' {} a -> s {id = a} :: CreateLinkResponse)

-- | The label that you assigned to this link. If the @labelTemplate@
-- includes variables, this field displays the variables resolved to their
-- actual values.
createLinkResponse_label :: Lens.Lens' CreateLinkResponse (Prelude.Maybe Prelude.Text)
createLinkResponse_label = Lens.lens (\CreateLinkResponse' {label} -> label) (\s@CreateLinkResponse' {} a -> s {label = a} :: CreateLinkResponse)

-- | The exact label template that you specified, with the variables not
-- resolved.
createLinkResponse_labelTemplate :: Lens.Lens' CreateLinkResponse (Prelude.Maybe Prelude.Text)
createLinkResponse_labelTemplate = Lens.lens (\CreateLinkResponse' {labelTemplate} -> labelTemplate) (\s@CreateLinkResponse' {} a -> s {labelTemplate = a} :: CreateLinkResponse)

-- | The resource types supported by this link.
createLinkResponse_resourceTypes :: Lens.Lens' CreateLinkResponse (Prelude.Maybe [Prelude.Text])
createLinkResponse_resourceTypes = Lens.lens (\CreateLinkResponse' {resourceTypes} -> resourceTypes) (\s@CreateLinkResponse' {} a -> s {resourceTypes = a} :: CreateLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the sink that is used for this link.
createLinkResponse_sinkArn :: Lens.Lens' CreateLinkResponse (Prelude.Maybe Prelude.Text)
createLinkResponse_sinkArn = Lens.lens (\CreateLinkResponse' {sinkArn} -> sinkArn) (\s@CreateLinkResponse' {} a -> s {sinkArn = a} :: CreateLinkResponse)

-- | The tags assigned to the link.
createLinkResponse_tags :: Lens.Lens' CreateLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLinkResponse_tags = Lens.lens (\CreateLinkResponse' {tags} -> tags) (\s@CreateLinkResponse' {} a -> s {tags = a} :: CreateLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createLinkResponse_httpStatus :: Lens.Lens' CreateLinkResponse Prelude.Int
createLinkResponse_httpStatus = Lens.lens (\CreateLinkResponse' {httpStatus} -> httpStatus) (\s@CreateLinkResponse' {} a -> s {httpStatus = a} :: CreateLinkResponse)

instance Prelude.NFData CreateLinkResponse where
  rnf CreateLinkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf labelTemplate
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
