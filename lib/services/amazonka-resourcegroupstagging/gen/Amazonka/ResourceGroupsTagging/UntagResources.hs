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
-- Module      : Amazonka.ResourceGroupsTagging.UntagResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- -   You can only tag resources that are located in the specified Amazon
--     Web Services Region for the calling Amazon Web Services account.
--
-- __Minimum permissions__
--
-- In addition to the @tag:UntagResources@ permission required by this
-- operation, you must also have the remove tags permission defined by the
-- service that created the resource. For example, to remove the tags from
-- an Amazon EC2 instance using the @UntagResources@ operation, you must
-- have both of the following permissions:
--
-- -   @tag:UntagResource@
--
-- -   @ec2:DeleteTags@
module Amazonka.ResourceGroupsTagging.UntagResources
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroupsTagging.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagResources' smart constructor.
data UntagResources = UntagResources'
  { -- | Specifies a list of ARNs of the resources that you want to remove tags
    -- from.
    --
    -- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
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
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
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
        Lens.coerced Lens.# pResourceARNList_,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | Specifies a list of ARNs of the resources that you want to remove tags
-- from.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
untagResources_resourceARNList :: Lens.Lens' UntagResources (Prelude.NonEmpty Prelude.Text)
untagResources_resourceARNList = Lens.lens (\UntagResources' {resourceARNList} -> resourceARNList) (\s@UntagResources' {} a -> s {resourceARNList = a} :: UntagResources) Prelude.. Lens.coerced

-- | Specifies a list of tag keys that you want to remove from the specified
-- resources.
untagResources_tagKeys :: Lens.Lens' UntagResources (Prelude.NonEmpty Prelude.Text)
untagResources_tagKeys = Lens.lens (\UntagResources' {tagKeys} -> tagKeys) (\s@UntagResources' {} a -> s {tagKeys = a} :: UntagResources) Prelude.. Lens.coerced

instance Core.AWSRequest UntagResources where
  type
    AWSResponse UntagResources =
      UntagResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResourcesResponse'
            Prelude.<$> ( x
                            Data..?> "FailedResourcesMap"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagResources where
  hashWithSalt _salt UntagResources' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARNList
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResources where
  rnf UntagResources' {..} =
    Prelude.rnf resourceARNList
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ResourceGroupsTaggingAPI_20170126.UntagResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UntagResources where
  toJSON UntagResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNList" Data..= resourceARNList),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath UntagResources where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagResources where
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
untagResourcesResponse_failedResourcesMap = Lens.lens (\UntagResourcesResponse' {failedResourcesMap} -> failedResourcesMap) (\s@UntagResourcesResponse' {} a -> s {failedResourcesMap = a} :: UntagResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
untagResourcesResponse_httpStatus :: Lens.Lens' UntagResourcesResponse Prelude.Int
untagResourcesResponse_httpStatus = Lens.lens (\UntagResourcesResponse' {httpStatus} -> httpStatus) (\s@UntagResourcesResponse' {} a -> s {httpStatus = a} :: UntagResourcesResponse)

instance Prelude.NFData UntagResourcesResponse where
  rnf UntagResourcesResponse' {..} =
    Prelude.rnf failedResourcesMap
      `Prelude.seq` Prelude.rnf httpStatus
