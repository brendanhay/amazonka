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
-- Module      : Amazonka.CloudHSM.AddTagsToResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Adds or overwrites one or more tags for the specified AWS CloudHSM
-- resource.
--
-- Each tag consists of a key and a value. Tag keys must be unique to each
-- resource.
module Amazonka.CloudHSM.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceArn,
    addTagsToResource_tagList,

    -- * Destructuring the Response
    AddTagsToResourceResponse (..),
    newAddTagsToResourceResponse,

    -- * Response Lenses
    addTagsToResourceResponse_httpStatus,
    addTagsToResourceResponse_status,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
    resourceArn :: Prelude.Text,
    -- | One or more tags.
    tagList :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'addTagsToResource_resourceArn' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
--
-- 'tagList', 'addTagsToResource_tagList' - One or more tags.
newAddTagsToResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  AddTagsToResource
newAddTagsToResource pResourceArn_ =
  AddTagsToResource'
    { resourceArn = pResourceArn_,
      tagList = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
addTagsToResource_resourceArn :: Lens.Lens' AddTagsToResource Prelude.Text
addTagsToResource_resourceArn = Lens.lens (\AddTagsToResource' {resourceArn} -> resourceArn) (\s@AddTagsToResource' {} a -> s {resourceArn = a} :: AddTagsToResource)

-- | One or more tags.
addTagsToResource_tagList :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tagList = Lens.lens (\AddTagsToResource' {tagList} -> tagList) (\s@AddTagsToResource' {} a -> s {tagList = a} :: AddTagsToResource) Prelude.. Lens.coerced

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsToResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable AddTagsToResource where
  hashWithSalt _salt AddTagsToResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tagList

instance Prelude.NFData AddTagsToResource where
  rnf AddTagsToResource' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf tagList

instance Data.ToHeaders AddTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.AddTagsToResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("TagList" Data..= tagList)
          ]
      )

instance Data.ToPath AddTagsToResource where
  toPath = Prelude.const "/"

instance Data.ToQuery AddTagsToResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the operation.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsToResourceResponse_httpStatus' - The response's http status code.
--
-- 'status', 'addTagsToResourceResponse_status' - The status of the operation.
newAddTagsToResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  Prelude.Text ->
  AddTagsToResourceResponse
newAddTagsToResourceResponse pHttpStatus_ pStatus_ =
  AddTagsToResourceResponse'
    { httpStatus =
        pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
addTagsToResourceResponse_httpStatus :: Lens.Lens' AddTagsToResourceResponse Prelude.Int
addTagsToResourceResponse_httpStatus = Lens.lens (\AddTagsToResourceResponse' {httpStatus} -> httpStatus) (\s@AddTagsToResourceResponse' {} a -> s {httpStatus = a} :: AddTagsToResourceResponse)

-- | The status of the operation.
addTagsToResourceResponse_status :: Lens.Lens' AddTagsToResourceResponse Prelude.Text
addTagsToResourceResponse_status = Lens.lens (\AddTagsToResourceResponse' {status} -> status) (\s@AddTagsToResourceResponse' {} a -> s {status = a} :: AddTagsToResourceResponse)

instance Prelude.NFData AddTagsToResourceResponse where
  rnf AddTagsToResourceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
