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
-- Module      : Network.AWS.CloudHSM.RemoveTagsFromResource
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
-- Removes one or more tags from the specified AWS CloudHSM resource.
--
-- To remove a tag, specify only the tag key to remove (not the value). To
-- overwrite the value for an existing tag, use AddTagsToResource.
module Network.AWS.CloudHSM.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceArn,
    removeTagsFromResource_tagKeyList,

    -- * Destructuring the Response
    RemoveTagsFromResourceResponse (..),
    newRemoveTagsFromResourceResponse,

    -- * Response Lenses
    removeTagsFromResourceResponse_httpStatus,
    removeTagsFromResourceResponse_status,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
    resourceArn :: Prelude.Text,
    -- | The tag key or keys to remove.
    --
    -- Specify only the tag key to remove (not the value). To overwrite the
    -- value for an existing tag, use AddTagsToResource.
    tagKeyList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'removeTagsFromResource_resourceArn' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
--
-- 'tagKeyList', 'removeTagsFromResource_tagKeyList' - The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the
-- value for an existing tag, use AddTagsToResource.
newRemoveTagsFromResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceArn_ =
  RemoveTagsFromResource'
    { resourceArn =
        pResourceArn_,
      tagKeyList = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
removeTagsFromResource_resourceArn :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceArn = Lens.lens (\RemoveTagsFromResource' {resourceArn} -> resourceArn) (\s@RemoveTagsFromResource' {} a -> s {resourceArn = a} :: RemoveTagsFromResource)

-- | The tag key or keys to remove.
--
-- Specify only the tag key to remove (not the value). To overwrite the
-- value for an existing tag, use AddTagsToResource.
removeTagsFromResource_tagKeyList :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeyList = Lens.lens (\RemoveTagsFromResource' {tagKeyList} -> tagKeyList) (\s@RemoveTagsFromResource' {} a -> s {tagKeyList = a} :: RemoveTagsFromResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RemoveTagsFromResource where
  type
    Rs RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Status")
      )

instance Prelude.Hashable RemoveTagsFromResource

instance Prelude.NFData RemoveTagsFromResource

instance Prelude.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.RemoveTagsFromResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Prelude..= resourceArn),
            Prelude.Just ("TagKeyList" Prelude..= tagKeyList)
          ]
      )

instance Prelude.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveTagsFromResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the operation.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsFromResourceResponse_httpStatus' - The response's http status code.
--
-- 'status', 'removeTagsFromResourceResponse_status' - The status of the operation.
newRemoveTagsFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  Prelude.Text ->
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse
  pHttpStatus_
  pStatus_ =
    RemoveTagsFromResourceResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
removeTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveTagsFromResourceResponse Prelude.Int
removeTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveTagsFromResourceResponse)

-- | The status of the operation.
removeTagsFromResourceResponse_status :: Lens.Lens' RemoveTagsFromResourceResponse Prelude.Text
removeTagsFromResourceResponse_status = Lens.lens (\RemoveTagsFromResourceResponse' {status} -> status) (\s@RemoveTagsFromResourceResponse' {} a -> s {status = a} :: RemoveTagsFromResourceResponse)

instance
  Prelude.NFData
    RemoveTagsFromResourceResponse
