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
-- Module      : Network.AWS.DMS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an AWS DMS resource, including replication
-- instance, endpoint, security group, and migration task. For more
-- information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html Tag>
-- data type description.
module Network.AWS.DMS.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceArn,
    removeTagsFromResource_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromResourceResponse (..),
    newRemoveTagsFromResourceResponse,

    -- * Response Lenses
    removeTagsFromResourceResponse_httpStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes one or more tags from an AWS DMS resource.
--
-- /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | An AWS DMS resource from which you want to remove tag(s). The value for
    -- this parameter is an Amazon Resource Name (ARN).
    resourceArn :: Prelude.Text,
    -- | The tag key (name) of the tag to be removed.
    tagKeys :: [Prelude.Text]
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
-- 'resourceArn', 'removeTagsFromResource_resourceArn' - An AWS DMS resource from which you want to remove tag(s). The value for
-- this parameter is an Amazon Resource Name (ARN).
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - The tag key (name) of the tag to be removed.
newRemoveTagsFromResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceArn_ =
  RemoveTagsFromResource'
    { resourceArn =
        pResourceArn_,
      tagKeys = Prelude.mempty
    }

-- | An AWS DMS resource from which you want to remove tag(s). The value for
-- this parameter is an Amazon Resource Name (ARN).
removeTagsFromResource_resourceArn :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceArn = Lens.lens (\RemoveTagsFromResource' {resourceArn} -> resourceArn) (\s@RemoveTagsFromResource' {} a -> s {resourceArn = a} :: RemoveTagsFromResource)

-- | The tag key (name) of the tag to be removed.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest RemoveTagsFromResource where
  type
    Rs RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTagsFromResource

instance Prelude.NFData RemoveTagsFromResource

instance Prelude.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.RemoveTagsFromResource" ::
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
            Prelude.Just ("TagKeys" Prelude..= tagKeys)
          ]
      )

instance Prelude.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveTagsFromResource where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
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
newRemoveTagsFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse pHttpStatus_ =
  RemoveTagsFromResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveTagsFromResourceResponse Prelude.Int
removeTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveTagsFromResourceResponse)

instance
  Prelude.NFData
    RemoveTagsFromResourceResponse
