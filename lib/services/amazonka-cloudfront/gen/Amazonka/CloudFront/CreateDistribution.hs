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
-- Module      : Amazonka.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CloudFront distribution.
module Amazonka.CloudFront.CreateDistribution
  ( -- * Creating a Request
    CreateDistribution (..),
    newCreateDistribution,

    -- * Request Lenses
    createDistribution_distributionConfig,

    -- * Destructuring the Response
    CreateDistributionResponse (..),
    newCreateDistributionResponse,

    -- * Response Lenses
    createDistributionResponse_distribution,
    createDistributionResponse_eTag,
    createDistributionResponse_location,
    createDistributionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to create a new distribution.
--
-- /See:/ 'newCreateDistribution' smart constructor.
data CreateDistribution = CreateDistribution'
  { -- | The distribution\'s configuration information.
    distributionConfig :: DistributionConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionConfig', 'createDistribution_distributionConfig' - The distribution\'s configuration information.
newCreateDistribution ::
  -- | 'distributionConfig'
  DistributionConfig ->
  CreateDistribution
newCreateDistribution pDistributionConfig_ =
  CreateDistribution'
    { distributionConfig =
        pDistributionConfig_
    }

-- | The distribution\'s configuration information.
createDistribution_distributionConfig :: Lens.Lens' CreateDistribution DistributionConfig
createDistribution_distributionConfig = Lens.lens (\CreateDistribution' {distributionConfig} -> distributionConfig) (\s@CreateDistribution' {} a -> s {distributionConfig = a} :: CreateDistribution)

instance Core.AWSRequest CreateDistribution where
  type
    AWSResponse CreateDistribution =
      CreateDistributionResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDistributionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDistribution where
  hashWithSalt _salt CreateDistribution' {..} =
    _salt `Prelude.hashWithSalt` distributionConfig

instance Prelude.NFData CreateDistribution where
  rnf CreateDistribution' {..} =
    Prelude.rnf distributionConfig

instance Data.ToElement CreateDistribution where
  toElement CreateDistribution' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      distributionConfig

instance Data.ToHeaders CreateDistribution where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDistribution where
  toPath = Prelude.const "/2020-05-31/distribution"

instance Data.ToQuery CreateDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
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
-- Create a value of 'CreateDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distribution', 'createDistributionResponse_distribution' - The distribution\'s information.
--
-- 'eTag', 'createDistributionResponse_eTag' - The current version of the distribution created.
--
-- 'location', 'createDistributionResponse_location' - The fully qualified URI of the new distribution resource just created.
--
-- 'httpStatus', 'createDistributionResponse_httpStatus' - The response's http status code.
newCreateDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDistributionResponse
newCreateDistributionResponse pHttpStatus_ =
  CreateDistributionResponse'
    { distribution =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The distribution\'s information.
createDistributionResponse_distribution :: Lens.Lens' CreateDistributionResponse (Prelude.Maybe Distribution)
createDistributionResponse_distribution = Lens.lens (\CreateDistributionResponse' {distribution} -> distribution) (\s@CreateDistributionResponse' {} a -> s {distribution = a} :: CreateDistributionResponse)

-- | The current version of the distribution created.
createDistributionResponse_eTag :: Lens.Lens' CreateDistributionResponse (Prelude.Maybe Prelude.Text)
createDistributionResponse_eTag = Lens.lens (\CreateDistributionResponse' {eTag} -> eTag) (\s@CreateDistributionResponse' {} a -> s {eTag = a} :: CreateDistributionResponse)

-- | The fully qualified URI of the new distribution resource just created.
createDistributionResponse_location :: Lens.Lens' CreateDistributionResponse (Prelude.Maybe Prelude.Text)
createDistributionResponse_location = Lens.lens (\CreateDistributionResponse' {location} -> location) (\s@CreateDistributionResponse' {} a -> s {location = a} :: CreateDistributionResponse)

-- | The response's http status code.
createDistributionResponse_httpStatus :: Lens.Lens' CreateDistributionResponse Prelude.Int
createDistributionResponse_httpStatus = Lens.lens (\CreateDistributionResponse' {httpStatus} -> httpStatus) (\s@CreateDistributionResponse' {} a -> s {httpStatus = a} :: CreateDistributionResponse)

instance Prelude.NFData CreateDistributionResponse where
  rnf CreateDistributionResponse' {..} =
    Prelude.rnf distribution `Prelude.seq`
      Prelude.rnf eTag `Prelude.seq`
        Prelude.rnf location `Prelude.seq`
          Prelude.rnf httpStatus
