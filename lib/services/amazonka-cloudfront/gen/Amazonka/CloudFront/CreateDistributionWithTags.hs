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
-- Module      : Amazonka.CloudFront.CreateDistributionWithTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new distribution with tags.
module Amazonka.CloudFront.CreateDistributionWithTags
  ( -- * Creating a Request
    CreateDistributionWithTags (..),
    newCreateDistributionWithTags,

    -- * Request Lenses
    createDistributionWithTags_distributionConfigWithTags,

    -- * Destructuring the Response
    CreateDistributionWithTagsResponse (..),
    newCreateDistributionWithTagsResponse,

    -- * Response Lenses
    createDistributionWithTagsResponse_distribution,
    createDistributionWithTagsResponse_eTag,
    createDistributionWithTagsResponse_location,
    createDistributionWithTagsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDistributionWithTagsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDistributionWithTags where
  hashWithSalt _salt CreateDistributionWithTags' {..} =
    _salt
      `Prelude.hashWithSalt` distributionConfigWithTags

instance Prelude.NFData CreateDistributionWithTags where
  rnf CreateDistributionWithTags' {..} =
    Prelude.rnf distributionConfigWithTags

instance Data.ToElement CreateDistributionWithTags where
  toElement CreateDistributionWithTags' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfigWithTags"
      distributionConfigWithTags

instance Data.ToHeaders CreateDistributionWithTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDistributionWithTags where
  toPath = Prelude.const "/2020-05-31/distribution"

instance Data.ToQuery CreateDistributionWithTags where
  toQuery =
    Prelude.const (Prelude.mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateDistributionWithTagsResponse' smart constructor.
data CreateDistributionWithTagsResponse = CreateDistributionWithTagsResponse'
  { -- | The distribution\'s information.
    distribution :: Prelude.Maybe Distribution,
    -- | The current version of the distribution created.
    eTag :: Prelude.Maybe Prelude.Text,
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
-- 'distribution', 'createDistributionWithTagsResponse_distribution' - The distribution\'s information.
--
-- 'eTag', 'createDistributionWithTagsResponse_eTag' - The current version of the distribution created.
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
    { distribution =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The distribution\'s information.
createDistributionWithTagsResponse_distribution :: Lens.Lens' CreateDistributionWithTagsResponse (Prelude.Maybe Distribution)
createDistributionWithTagsResponse_distribution = Lens.lens (\CreateDistributionWithTagsResponse' {distribution} -> distribution) (\s@CreateDistributionWithTagsResponse' {} a -> s {distribution = a} :: CreateDistributionWithTagsResponse)

-- | The current version of the distribution created.
createDistributionWithTagsResponse_eTag :: Lens.Lens' CreateDistributionWithTagsResponse (Prelude.Maybe Prelude.Text)
createDistributionWithTagsResponse_eTag = Lens.lens (\CreateDistributionWithTagsResponse' {eTag} -> eTag) (\s@CreateDistributionWithTagsResponse' {} a -> s {eTag = a} :: CreateDistributionWithTagsResponse)

-- | The fully qualified URI of the new distribution resource just created.
createDistributionWithTagsResponse_location :: Lens.Lens' CreateDistributionWithTagsResponse (Prelude.Maybe Prelude.Text)
createDistributionWithTagsResponse_location = Lens.lens (\CreateDistributionWithTagsResponse' {location} -> location) (\s@CreateDistributionWithTagsResponse' {} a -> s {location = a} :: CreateDistributionWithTagsResponse)

-- | The response's http status code.
createDistributionWithTagsResponse_httpStatus :: Lens.Lens' CreateDistributionWithTagsResponse Prelude.Int
createDistributionWithTagsResponse_httpStatus = Lens.lens (\CreateDistributionWithTagsResponse' {httpStatus} -> httpStatus) (\s@CreateDistributionWithTagsResponse' {} a -> s {httpStatus = a} :: CreateDistributionWithTagsResponse)

instance
  Prelude.NFData
    CreateDistributionWithTagsResponse
  where
  rnf CreateDistributionWithTagsResponse' {..} =
    Prelude.rnf distribution `Prelude.seq`
      Prelude.rnf eTag `Prelude.seq`
        Prelude.rnf location `Prelude.seq`
          Prelude.rnf httpStatus
