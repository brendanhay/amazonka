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
-- Module      : Amazonka.ResourceGroupsTagging.TagResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies one or more tags to the specified resources. Note the following:
--
-- -   Not all resources can have tags. For a list of services with
--     resources that support tagging using this operation, see
--     <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
--     If the resource doesn\'t yet support this operation, the resource\'s
--     service might support tagging using its own API operations. For more
--     information, refer to the documentation for that service.
--
-- -   Each resource can have up to 50 tags. For other limits, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag Naming and Usage Conventions>
--     in the /Amazon Web Services General Reference./
--
-- -   You can only tag resources that are located in the specified Amazon
--     Web Services Region for the Amazon Web Services account.
--
-- -   To add tags to a resource, you need the necessary permissions for
--     the service that the resource belongs to as well as permissions for
--     adding tags. For more information, see the documentation for each
--     service.
--
-- Do not store personally identifiable information (PII) or other
-- confidential or sensitive information in tags. We use tags to provide
-- you with billing and administration services. Tags are not intended to
-- be used for private or sensitive data.
--
-- __Minimum permissions__
--
-- In addition to the @tag:TagResources@ permission required by this
-- operation, you must also have the tagging permission defined by the
-- service that created the resource. For example, to tag an Amazon EC2
-- instance using the @TagResources@ operation, you must have both of the
-- following permissions:
--
-- -   @tag:TagResource@
--
-- -   @ec2:CreateTags@
module Amazonka.ResourceGroupsTagging.TagResources
  ( -- * Creating a Request
    TagResources (..),
    newTagResources,

    -- * Request Lenses
    tagResources_resourceARNList,
    tagResources_tags,

    -- * Destructuring the Response
    TagResourcesResponse (..),
    newTagResourcesResponse,

    -- * Response Lenses
    tagResourcesResponse_failedResourcesMap,
    tagResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroupsTagging.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResources' smart constructor.
data TagResources = TagResources'
  { -- | Specifies the list of ARNs of the resources that you want to apply tags
    -- to.
    --
    -- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    resourceARNList :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies a list of tags that you want to add to the specified
    -- resources. A tag consists of a key and a value that you define.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNList', 'tagResources_resourceARNList' - Specifies the list of ARNs of the resources that you want to apply tags
-- to.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
--
-- 'tags', 'tagResources_tags' - Specifies a list of tags that you want to add to the specified
-- resources. A tag consists of a key and a value that you define.
newTagResources ::
  -- | 'resourceARNList'
  Prelude.NonEmpty Prelude.Text ->
  TagResources
newTagResources pResourceARNList_ =
  TagResources'
    { resourceARNList =
        Lens.coerced Lens.# pResourceARNList_,
      tags = Prelude.mempty
    }

-- | Specifies the list of ARNs of the resources that you want to apply tags
-- to.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
tagResources_resourceARNList :: Lens.Lens' TagResources (Prelude.NonEmpty Prelude.Text)
tagResources_resourceARNList = Lens.lens (\TagResources' {resourceARNList} -> resourceARNList) (\s@TagResources' {} a -> s {resourceARNList = a} :: TagResources) Prelude.. Lens.coerced

-- | Specifies a list of tags that you want to add to the specified
-- resources. A tag consists of a key and a value that you define.
tagResources_tags :: Lens.Lens' TagResources (Prelude.HashMap Prelude.Text Prelude.Text)
tagResources_tags = Lens.lens (\TagResources' {tags} -> tags) (\s@TagResources' {} a -> s {tags = a} :: TagResources) Prelude.. Lens.coerced

instance Core.AWSRequest TagResources where
  type AWSResponse TagResources = TagResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TagResourcesResponse'
            Prelude.<$> ( x
                            Data..?> "FailedResourcesMap"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResources where
  hashWithSalt _salt TagResources' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARNList
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagResources where
  rnf TagResources' {..} =
    Prelude.rnf resourceARNList
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ResourceGroupsTaggingAPI_20170126.TagResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TagResources where
  toJSON TagResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNList" Data..= resourceARNList),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath TagResources where
  toPath = Prelude.const "/"

instance Data.ToQuery TagResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourcesResponse' smart constructor.
data TagResourcesResponse = TagResourcesResponse'
  { -- | A map containing a key-value pair for each failed item that couldn\'t be
    -- tagged. The key is the ARN of the failed resource. The value is a
    -- @FailureInfo@ object that contains an error code, a status code, and an
    -- error message. If there are no errors, the @FailedResourcesMap@ is
    -- empty.
    failedResourcesMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text FailureInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedResourcesMap', 'tagResourcesResponse_failedResourcesMap' - A map containing a key-value pair for each failed item that couldn\'t be
-- tagged. The key is the ARN of the failed resource. The value is a
-- @FailureInfo@ object that contains an error code, a status code, and an
-- error message. If there are no errors, the @FailedResourcesMap@ is
-- empty.
--
-- 'httpStatus', 'tagResourcesResponse_httpStatus' - The response's http status code.
newTagResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TagResourcesResponse
newTagResourcesResponse pHttpStatus_ =
  TagResourcesResponse'
    { failedResourcesMap =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map containing a key-value pair for each failed item that couldn\'t be
-- tagged. The key is the ARN of the failed resource. The value is a
-- @FailureInfo@ object that contains an error code, a status code, and an
-- error message. If there are no errors, the @FailedResourcesMap@ is
-- empty.
tagResourcesResponse_failedResourcesMap :: Lens.Lens' TagResourcesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text FailureInfo))
tagResourcesResponse_failedResourcesMap = Lens.lens (\TagResourcesResponse' {failedResourcesMap} -> failedResourcesMap) (\s@TagResourcesResponse' {} a -> s {failedResourcesMap = a} :: TagResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
tagResourcesResponse_httpStatus :: Lens.Lens' TagResourcesResponse Prelude.Int
tagResourcesResponse_httpStatus = Lens.lens (\TagResourcesResponse' {httpStatus} -> httpStatus) (\s@TagResourcesResponse' {} a -> s {httpStatus = a} :: TagResourcesResponse)

instance Prelude.NFData TagResourcesResponse where
  rnf TagResourcesResponse' {..} =
    Prelude.rnf failedResourcesMap
      `Prelude.seq` Prelude.rnf httpStatus
