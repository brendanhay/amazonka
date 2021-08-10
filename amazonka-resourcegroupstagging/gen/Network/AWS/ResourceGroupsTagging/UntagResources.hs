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
-- Module      : Network.AWS.ResourceGroupsTagging.UntagResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified resources. When you
-- specify a tag key, the action removes both that key and its associated
-- value. The operation succeeds even if you attempt to remove tags from a
-- resource that were already removed. Note the following:
--
-- -   To remove tags from a resource, you need the necessary permissions
--     for the service that the resource belongs to as well as permissions
--     for removing tags. For more information, see the documentation for
--     the service whose resource you want to untag.
--
-- -   You can only tag resources that are located in the specified AWS
--     Region for the calling AWS account.
module Network.AWS.ResourceGroupsTagging.UntagResources
  ( -- * Creating a Request
    UntagResources (..),
    newUntagResources,

    -- * Request Lenses
    untagResources_resourceARNList,
    untagResources_tagKeys,

    -- * Destructuring the Response
    UntagResourcesResponse (..),
    newUntagResourcesResponse,

    -- * Response Lenses
    untagResourcesResponse_failedResourcesMap,
    untagResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagResources' smart constructor.
data UntagResources = UntagResources'
  { -- | Specifies a list of ARNs of the resources that you want to remove tags
    -- from.
    --
    -- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
    -- information, see
    -- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    resourceARNList :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies a list of tag keys that you want to remove from the specified
    -- resources.
    tagKeys :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNList', 'untagResources_resourceARNList' - Specifies a list of ARNs of the resources that you want to remove tags
-- from.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'tagKeys', 'untagResources_tagKeys' - Specifies a list of tag keys that you want to remove from the specified
-- resources.
newUntagResources ::
  -- | 'resourceARNList'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  UntagResources
newUntagResources pResourceARNList_ pTagKeys_ =
  UntagResources'
    { resourceARNList =
        Lens._Coerce Lens.# pResourceARNList_,
      tagKeys = Lens._Coerce Lens.# pTagKeys_
    }

-- | Specifies a list of ARNs of the resources that you want to remove tags
-- from.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
untagResources_resourceARNList :: Lens.Lens' UntagResources (Prelude.NonEmpty Prelude.Text)
untagResources_resourceARNList = Lens.lens (\UntagResources' {resourceARNList} -> resourceARNList) (\s@UntagResources' {} a -> s {resourceARNList = a} :: UntagResources) Prelude.. Lens._Coerce

-- | Specifies a list of tag keys that you want to remove from the specified
-- resources.
untagResources_tagKeys :: Lens.Lens' UntagResources (Prelude.NonEmpty Prelude.Text)
untagResources_tagKeys = Lens.lens (\UntagResources' {tagKeys} -> tagKeys) (\s@UntagResources' {} a -> s {tagKeys = a} :: UntagResources) Prelude.. Lens._Coerce

instance Core.AWSRequest UntagResources where
  type
    AWSResponse UntagResources =
      UntagResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResourcesResponse'
            Prelude.<$> ( x Core..?> "FailedResourcesMap"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagResources

instance Prelude.NFData UntagResources

instance Core.ToHeaders UntagResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.UntagResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UntagResources where
  toJSON UntagResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNList" Core..= resourceARNList),
            Prelude.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath UntagResources where
  toPath = Prelude.const "/"

instance Core.ToQuery UntagResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourcesResponse' smart constructor.
data UntagResourcesResponse = UntagResourcesResponse'
  { -- | A map containing a key-value pair for each failed item that couldn\'t be
    -- untagged. The key is the ARN of the failed resource. The value is a
    -- @FailureInfo@ object that contains an error code, a status code, and an
    -- error message. If there are no errors, the @FailedResourcesMap@ is
    -- empty.
    failedResourcesMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text FailureInfo),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedResourcesMap', 'untagResourcesResponse_failedResourcesMap' - A map containing a key-value pair for each failed item that couldn\'t be
-- untagged. The key is the ARN of the failed resource. The value is a
-- @FailureInfo@ object that contains an error code, a status code, and an
-- error message. If there are no errors, the @FailedResourcesMap@ is
-- empty.
--
-- 'httpStatus', 'untagResourcesResponse_httpStatus' - The response's http status code.
newUntagResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UntagResourcesResponse
newUntagResourcesResponse pHttpStatus_ =
  UntagResourcesResponse'
    { failedResourcesMap =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map containing a key-value pair for each failed item that couldn\'t be
-- untagged. The key is the ARN of the failed resource. The value is a
-- @FailureInfo@ object that contains an error code, a status code, and an
-- error message. If there are no errors, the @FailedResourcesMap@ is
-- empty.
untagResourcesResponse_failedResourcesMap :: Lens.Lens' UntagResourcesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text FailureInfo))
untagResourcesResponse_failedResourcesMap = Lens.lens (\UntagResourcesResponse' {failedResourcesMap} -> failedResourcesMap) (\s@UntagResourcesResponse' {} a -> s {failedResourcesMap = a} :: UntagResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
untagResourcesResponse_httpStatus :: Lens.Lens' UntagResourcesResponse Prelude.Int
untagResourcesResponse_httpStatus = Lens.lens (\UntagResourcesResponse' {httpStatus} -> httpStatus) (\s@UntagResourcesResponse' {} a -> s {httpStatus = a} :: UntagResourcesResponse)

instance Prelude.NFData UntagResourcesResponse
