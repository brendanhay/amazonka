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
-- Module      : Amazonka.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about a distribution.
module Amazonka.CloudFront.GetDistribution
  ( -- * Creating a Request
    GetDistribution (..),
    newGetDistribution,

    -- * Request Lenses
    getDistribution_id,

    -- * Destructuring the Response
    GetDistributionResponse (..),
    newGetDistributionResponse,

    -- * Response Lenses
    getDistributionResponse_distribution,
    getDistributionResponse_eTag,
    getDistributionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to get a distribution\'s information.
--
-- /See:/ 'newGetDistribution' smart constructor.
data GetDistribution = GetDistribution'
  { -- | The distribution\'s ID. If the ID is empty, an empty distribution
    -- configuration is returned.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getDistribution_id' - The distribution\'s ID. If the ID is empty, an empty distribution
-- configuration is returned.
newGetDistribution ::
  -- | 'id'
  Prelude.Text ->
  GetDistribution
newGetDistribution pId_ = GetDistribution' {id = pId_}

-- | The distribution\'s ID. If the ID is empty, an empty distribution
-- configuration is returned.
getDistribution_id :: Lens.Lens' GetDistribution Prelude.Text
getDistribution_id = Lens.lens (\GetDistribution' {id} -> id) (\s@GetDistribution' {} a -> s {id = a} :: GetDistribution)

instance Core.AWSRequest GetDistribution where
  type
    AWSResponse GetDistribution =
      GetDistributionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetDistributionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDistribution where
  hashWithSalt _salt GetDistribution' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetDistribution where
  rnf GetDistribution' {..} = Prelude.rnf id

instance Data.ToHeaders GetDistribution where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDistribution where
  toPath GetDistribution' {..} =
    Prelude.mconcat
      ["/2020-05-31/distribution/", Data.toBS id]

instance Data.ToQuery GetDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newGetDistributionResponse' smart constructor.
data GetDistributionResponse = GetDistributionResponse'
  { -- | The distribution\'s information.
    distribution :: Prelude.Maybe Distribution,
    -- | The current version of the distribution\'s information. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distribution', 'getDistributionResponse_distribution' - The distribution\'s information.
--
-- 'eTag', 'getDistributionResponse_eTag' - The current version of the distribution\'s information. For example:
-- @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'getDistributionResponse_httpStatus' - The response's http status code.
newGetDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionResponse
newGetDistributionResponse pHttpStatus_ =
  GetDistributionResponse'
    { distribution =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The distribution\'s information.
getDistributionResponse_distribution :: Lens.Lens' GetDistributionResponse (Prelude.Maybe Distribution)
getDistributionResponse_distribution = Lens.lens (\GetDistributionResponse' {distribution} -> distribution) (\s@GetDistributionResponse' {} a -> s {distribution = a} :: GetDistributionResponse)

-- | The current version of the distribution\'s information. For example:
-- @E2QWRUHAPOMQZL@.
getDistributionResponse_eTag :: Lens.Lens' GetDistributionResponse (Prelude.Maybe Prelude.Text)
getDistributionResponse_eTag = Lens.lens (\GetDistributionResponse' {eTag} -> eTag) (\s@GetDistributionResponse' {} a -> s {eTag = a} :: GetDistributionResponse)

-- | The response's http status code.
getDistributionResponse_httpStatus :: Lens.Lens' GetDistributionResponse Prelude.Int
getDistributionResponse_httpStatus = Lens.lens (\GetDistributionResponse' {httpStatus} -> httpStatus) (\s@GetDistributionResponse' {} a -> s {httpStatus = a} :: GetDistributionResponse)

instance Prelude.NFData GetDistributionResponse where
  rnf GetDistributionResponse' {..} =
    Prelude.rnf distribution
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
