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
-- Module      : Network.AWS.CloudFront.CreateDistributionWithTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new distribution with tags.
module Network.AWS.CloudFront.CreateDistributionWithTags
  ( -- * Creating a Request
    CreateDistributionWithTags (..),
    newCreateDistributionWithTags,

    -- * Request Lenses
    createDistributionWithTags_distributionConfigWithTags,

    -- * Destructuring the Response
    CreateDistributionWithTagsResponse (..),
    newCreateDistributionWithTagsResponse,

    -- * Response Lenses
    createDistributionWithTagsResponse_eTag,
    createDistributionWithTagsResponse_distribution,
    createDistributionWithTagsResponse_location,
    createDistributionWithTagsResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new distribution with tags.
--
-- /See:/ 'newCreateDistributionWithTags' smart constructor.
data CreateDistributionWithTags = CreateDistributionWithTags'
  { -- | The distribution\'s configuration information.
    distributionConfigWithTags :: DistributionConfigWithTags
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDistributionWithTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionConfigWithTags', 'createDistributionWithTags_distributionConfigWithTags' - The distribution\'s configuration information.
newCreateDistributionWithTags ::
  -- | 'distributionConfigWithTags'
  DistributionConfigWithTags ->
  CreateDistributionWithTags
newCreateDistributionWithTags
  pDistributionConfigWithTags_ =
    CreateDistributionWithTags'
      { distributionConfigWithTags =
          pDistributionConfigWithTags_
      }

-- | The distribution\'s configuration information.
createDistributionWithTags_distributionConfigWithTags :: Lens.Lens' CreateDistributionWithTags DistributionConfigWithTags
createDistributionWithTags_distributionConfigWithTags = Lens.lens (\CreateDistributionWithTags' {distributionConfigWithTags} -> distributionConfigWithTags) (\s@CreateDistributionWithTags' {} a -> s {distributionConfigWithTags = a} :: CreateDistributionWithTags)

instance Core.AWSRequest CreateDistributionWithTags where
  type
    AWSResponse CreateDistributionWithTags =
      CreateDistributionWithTagsResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDistributionWithTagsResponse'
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (h Core..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDistributionWithTags

instance Prelude.NFData CreateDistributionWithTags

instance Core.ToElement CreateDistributionWithTags where
  toElement CreateDistributionWithTags' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfigWithTags"
      distributionConfigWithTags

instance Core.ToHeaders CreateDistributionWithTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDistributionWithTags where
  toPath = Prelude.const "/2020-05-31/distribution"

instance Core.ToQuery CreateDistributionWithTags where
  toQuery =
    Prelude.const (Prelude.mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateDistributionWithTagsResponse' smart constructor.
data CreateDistributionWithTagsResponse = CreateDistributionWithTagsResponse'
  { -- | The current version of the distribution created.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The distribution\'s information.
    distribution :: Prelude.Maybe Distribution,
    -- | The fully qualified URI of the new distribution resource just created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDistributionWithTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createDistributionWithTagsResponse_eTag' - The current version of the distribution created.
--
-- 'distribution', 'createDistributionWithTagsResponse_distribution' - The distribution\'s information.
--
-- 'location', 'createDistributionWithTagsResponse_location' - The fully qualified URI of the new distribution resource just created.
--
-- 'httpStatus', 'createDistributionWithTagsResponse_httpStatus' - The response's http status code.
newCreateDistributionWithTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDistributionWithTagsResponse
newCreateDistributionWithTagsResponse pHttpStatus_ =
  CreateDistributionWithTagsResponse'
    { eTag =
        Prelude.Nothing,
      distribution = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the distribution created.
createDistributionWithTagsResponse_eTag :: Lens.Lens' CreateDistributionWithTagsResponse (Prelude.Maybe Prelude.Text)
createDistributionWithTagsResponse_eTag = Lens.lens (\CreateDistributionWithTagsResponse' {eTag} -> eTag) (\s@CreateDistributionWithTagsResponse' {} a -> s {eTag = a} :: CreateDistributionWithTagsResponse)

-- | The distribution\'s information.
createDistributionWithTagsResponse_distribution :: Lens.Lens' CreateDistributionWithTagsResponse (Prelude.Maybe Distribution)
createDistributionWithTagsResponse_distribution = Lens.lens (\CreateDistributionWithTagsResponse' {distribution} -> distribution) (\s@CreateDistributionWithTagsResponse' {} a -> s {distribution = a} :: CreateDistributionWithTagsResponse)

-- | The fully qualified URI of the new distribution resource just created.
createDistributionWithTagsResponse_location :: Lens.Lens' CreateDistributionWithTagsResponse (Prelude.Maybe Prelude.Text)
createDistributionWithTagsResponse_location = Lens.lens (\CreateDistributionWithTagsResponse' {location} -> location) (\s@CreateDistributionWithTagsResponse' {} a -> s {location = a} :: CreateDistributionWithTagsResponse)

-- | The response's http status code.
createDistributionWithTagsResponse_httpStatus :: Lens.Lens' CreateDistributionWithTagsResponse Prelude.Int
createDistributionWithTagsResponse_httpStatus = Lens.lens (\CreateDistributionWithTagsResponse' {httpStatus} -> httpStatus) (\s@CreateDistributionWithTagsResponse' {} a -> s {httpStatus = a} :: CreateDistributionWithTagsResponse)

instance
  Prelude.NFData
    CreateDistributionWithTagsResponse
