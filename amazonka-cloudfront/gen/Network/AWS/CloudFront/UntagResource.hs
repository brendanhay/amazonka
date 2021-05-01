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
-- Module      : Network.AWS.CloudFront.UntagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove tags from a CloudFront resource.
module Network.AWS.CloudFront.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resource,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to remove tags from a CloudFront resource.
--
-- /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Prelude.Text,
    -- | A complex type that contains zero or more @Tag@ key elements.
    tagKeys :: TagKeys
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'untagResource_resource' - An ARN of a CloudFront resource.
--
-- 'tagKeys', 'untagResource_tagKeys' - A complex type that contains zero or more @Tag@ key elements.
newUntagResource ::
  -- | 'resource'
  Prelude.Text ->
  -- | 'tagKeys'
  TagKeys ->
  UntagResource
newUntagResource pResource_ pTagKeys_ =
  UntagResource'
    { resource = pResource_,
      tagKeys = pTagKeys_
    }

-- | An ARN of a CloudFront resource.
untagResource_resource :: Lens.Lens' UntagResource Prelude.Text
untagResource_resource = Lens.lens (\UntagResource' {resource} -> resource) (\s@UntagResource' {} a -> s {resource = a} :: UntagResource)

-- | A complex type that contains zero or more @Tag@ key elements.
untagResource_tagKeys :: Lens.Lens' UntagResource TagKeys
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource)

instance Prelude.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request = Request.postXML defaultService
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource

instance Prelude.NFData UntagResource

instance Prelude.ToElement UntagResource where
  toElement UntagResource' {..} =
    Prelude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}TagKeys"
      tagKeys

instance Prelude.ToHeaders UntagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagResource where
  toPath = Prelude.const "/2020-05-31/tagging"

instance Prelude.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Prelude.mconcat
      ["Resource" Prelude.=: resource, "Operation=Untag"]

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Prelude.NFData UntagResourceResponse
