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
-- Module      : Amazonka.CloudFront.UntagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove tags from a CloudFront resource.
module Amazonka.CloudFront.UntagResource
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

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to remove tags from a CloudFront resource.
--
-- /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Prelude.Text,
    -- | A complex type that contains zero or more @Tag@ key elements.
    tagKeys :: TagKeys
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource where
  hashWithSalt _salt UntagResource' {..} =
    _salt
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf resource
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToElement UntagResource where
  toElement UntagResource' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}TagKeys"
      tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagResource where
  toPath = Prelude.const "/2020-05-31/tagging"

instance Data.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Prelude.mconcat
      ["Resource" Data.=: resource, "Operation=Untag"]

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Prelude.NFData UntagResourceResponse where
  rnf _ = ()
