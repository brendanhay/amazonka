{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudHSM.AddTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Adds or overwrites one or more tags for the specified AWS CloudHSM
-- resource.
--
-- Each tag consists of a key and a value. Tag keys must be unique to each
-- resource.
module Network.AWS.CloudHSM.AddTagsToResource
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

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource to tag.
    resourceArn :: Prelude.Text,
    -- | One or more tags.
    tagList :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
addTagsToResource_tagList = Lens.lens (\AddTagsToResource' {tagList} -> tagList) (\s@AddTagsToResource' {} a -> s {tagList = a} :: AddTagsToResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddTagsToResource where
  type Rs AddTagsToResource = AddTagsToResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsToResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Status")
      )

instance Prelude.Hashable AddTagsToResource

instance Prelude.NFData AddTagsToResource

instance Prelude.ToHeaders AddTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.AddTagsToResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddTagsToResource where
  toJSON AddTagsToResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Prelude..= resourceArn),
            Prelude.Just ("TagList" Prelude..= tagList)
          ]
      )

instance Prelude.ToPath AddTagsToResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddTagsToResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the operation.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AddTagsToResourceResponse
