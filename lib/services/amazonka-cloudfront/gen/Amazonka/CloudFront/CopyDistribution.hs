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
-- Module      : Amazonka.CloudFront.CopyDistribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a staging distribution using the configuration of the provided
-- primary distribution. A staging distribution is a copy of an existing
-- distribution (called the primary distribution) that you can use in a
-- continuous deployment workflow.
--
-- After you create a staging distribution, you can use
-- @UpdateDistribution@ to modify the staging distribution’s configuration.
-- Then you can use @CreateContinuousDeploymentPolicy@ to incrementally
-- move traffic to the staging distribution.
module Amazonka.CloudFront.CopyDistribution
  ( -- * Creating a Request
    CopyDistribution (..),
    newCopyDistribution,

    -- * Request Lenses
    copyDistribution_staging,
    copyDistribution_ifMatch,
    copyDistribution_primaryDistributionId,
    copyDistribution_callerReference,

    -- * Destructuring the Response
    CopyDistributionResponse (..),
    newCopyDistributionResponse,

    -- * Response Lenses
    copyDistributionResponse_location,
    copyDistributionResponse_distribution,
    copyDistributionResponse_eTag,
    copyDistributionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyDistribution' smart constructor.
data CopyDistribution = CopyDistribution'
  { -- | The type of distribution that your primary distribution will be copied
    -- to. The only valid value is @True@, indicating that you are copying to a
    -- staging distribution.
    staging :: Prelude.Maybe Prelude.Bool,
    -- | The version identifier of the primary distribution whose configuration
    -- you are copying. This is the @ETag@ value returned in the response to
    -- @GetDistribution@ and @GetDistributionConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the primary distribution whose configuration you are
    -- copying. To get a distribution ID, use @ListDistributions@.
    primaryDistributionId :: Prelude.Text,
    -- | A value that uniquely identifies a request to create a resource. This
    -- helps to prevent CloudFront from creating a duplicate resource if you
    -- accidentally resubmit an identical request.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staging', 'copyDistribution_staging' - The type of distribution that your primary distribution will be copied
-- to. The only valid value is @True@, indicating that you are copying to a
-- staging distribution.
--
-- 'ifMatch', 'copyDistribution_ifMatch' - The version identifier of the primary distribution whose configuration
-- you are copying. This is the @ETag@ value returned in the response to
-- @GetDistribution@ and @GetDistributionConfig@.
--
-- 'primaryDistributionId', 'copyDistribution_primaryDistributionId' - The identifier of the primary distribution whose configuration you are
-- copying. To get a distribution ID, use @ListDistributions@.
--
-- 'callerReference', 'copyDistribution_callerReference' - A value that uniquely identifies a request to create a resource. This
-- helps to prevent CloudFront from creating a duplicate resource if you
-- accidentally resubmit an identical request.
newCopyDistribution ::
  -- | 'primaryDistributionId'
  Prelude.Text ->
  -- | 'callerReference'
  Prelude.Text ->
  CopyDistribution
newCopyDistribution
  pPrimaryDistributionId_
  pCallerReference_ =
    CopyDistribution'
      { staging = Prelude.Nothing,
        ifMatch = Prelude.Nothing,
        primaryDistributionId = pPrimaryDistributionId_,
        callerReference = pCallerReference_
      }

-- | The type of distribution that your primary distribution will be copied
-- to. The only valid value is @True@, indicating that you are copying to a
-- staging distribution.
copyDistribution_staging :: Lens.Lens' CopyDistribution (Prelude.Maybe Prelude.Bool)
copyDistribution_staging = Lens.lens (\CopyDistribution' {staging} -> staging) (\s@CopyDistribution' {} a -> s {staging = a} :: CopyDistribution)

-- | The version identifier of the primary distribution whose configuration
-- you are copying. This is the @ETag@ value returned in the response to
-- @GetDistribution@ and @GetDistributionConfig@.
copyDistribution_ifMatch :: Lens.Lens' CopyDistribution (Prelude.Maybe Prelude.Text)
copyDistribution_ifMatch = Lens.lens (\CopyDistribution' {ifMatch} -> ifMatch) (\s@CopyDistribution' {} a -> s {ifMatch = a} :: CopyDistribution)

-- | The identifier of the primary distribution whose configuration you are
-- copying. To get a distribution ID, use @ListDistributions@.
copyDistribution_primaryDistributionId :: Lens.Lens' CopyDistribution Prelude.Text
copyDistribution_primaryDistributionId = Lens.lens (\CopyDistribution' {primaryDistributionId} -> primaryDistributionId) (\s@CopyDistribution' {} a -> s {primaryDistributionId = a} :: CopyDistribution)

-- | A value that uniquely identifies a request to create a resource. This
-- helps to prevent CloudFront from creating a duplicate resource if you
-- accidentally resubmit an identical request.
copyDistribution_callerReference :: Lens.Lens' CopyDistribution Prelude.Text
copyDistribution_callerReference = Lens.lens (\CopyDistribution' {callerReference} -> callerReference) (\s@CopyDistribution' {} a -> s {callerReference = a} :: CopyDistribution)

instance Core.AWSRequest CopyDistribution where
  type
    AWSResponse CopyDistribution =
      CopyDistributionResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CopyDistributionResponse'
            Prelude.<$> (h Data..#? "Location")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDistribution where
  hashWithSalt _salt CopyDistribution' {..} =
    _salt `Prelude.hashWithSalt` staging
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` primaryDistributionId
      `Prelude.hashWithSalt` callerReference

instance Prelude.NFData CopyDistribution where
  rnf CopyDistribution' {..} =
    Prelude.rnf staging
      `Prelude.seq` Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf primaryDistributionId
      `Prelude.seq` Prelude.rnf callerReference

instance Data.ToElement CopyDistribution where
  toElement =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CopyDistributionRequest"

instance Data.ToHeaders CopyDistribution where
  toHeaders CopyDistribution' {..} =
    Prelude.mconcat
      [ "Staging" Data.=# staging,
        "If-Match" Data.=# ifMatch
      ]

instance Data.ToPath CopyDistribution where
  toPath CopyDistribution' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Data.toBS primaryDistributionId,
        "/copy"
      ]

instance Data.ToQuery CopyDistribution where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CopyDistribution where
  toXML CopyDistribution' {..} =
    Prelude.mconcat
      ["CallerReference" Data.@= callerReference]

-- | /See:/ 'newCopyDistributionResponse' smart constructor.
data CopyDistributionResponse = CopyDistributionResponse'
  { -- | The URL of the staging distribution.
    location :: Prelude.Maybe Prelude.Text,
    distribution :: Prelude.Maybe Distribution,
    -- | The version identifier for the current version of the staging
    -- distribution.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'copyDistributionResponse_location' - The URL of the staging distribution.
--
-- 'distribution', 'copyDistributionResponse_distribution' - Undocumented member.
--
-- 'eTag', 'copyDistributionResponse_eTag' - The version identifier for the current version of the staging
-- distribution.
--
-- 'httpStatus', 'copyDistributionResponse_httpStatus' - The response's http status code.
newCopyDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyDistributionResponse
newCopyDistributionResponse pHttpStatus_ =
  CopyDistributionResponse'
    { location =
        Prelude.Nothing,
      distribution = Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL of the staging distribution.
copyDistributionResponse_location :: Lens.Lens' CopyDistributionResponse (Prelude.Maybe Prelude.Text)
copyDistributionResponse_location = Lens.lens (\CopyDistributionResponse' {location} -> location) (\s@CopyDistributionResponse' {} a -> s {location = a} :: CopyDistributionResponse)

-- | Undocumented member.
copyDistributionResponse_distribution :: Lens.Lens' CopyDistributionResponse (Prelude.Maybe Distribution)
copyDistributionResponse_distribution = Lens.lens (\CopyDistributionResponse' {distribution} -> distribution) (\s@CopyDistributionResponse' {} a -> s {distribution = a} :: CopyDistributionResponse)

-- | The version identifier for the current version of the staging
-- distribution.
copyDistributionResponse_eTag :: Lens.Lens' CopyDistributionResponse (Prelude.Maybe Prelude.Text)
copyDistributionResponse_eTag = Lens.lens (\CopyDistributionResponse' {eTag} -> eTag) (\s@CopyDistributionResponse' {} a -> s {eTag = a} :: CopyDistributionResponse)

-- | The response's http status code.
copyDistributionResponse_httpStatus :: Lens.Lens' CopyDistributionResponse Prelude.Int
copyDistributionResponse_httpStatus = Lens.lens (\CopyDistributionResponse' {httpStatus} -> httpStatus) (\s@CopyDistributionResponse' {} a -> s {httpStatus = a} :: CopyDistributionResponse)

instance Prelude.NFData CopyDistributionResponse where
  rnf CopyDistributionResponse' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf distribution
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
