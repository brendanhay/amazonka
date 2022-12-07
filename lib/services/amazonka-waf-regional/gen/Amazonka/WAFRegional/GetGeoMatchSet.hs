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
-- Module      : Amazonka.WAFRegional.GetGeoMatchSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns the GeoMatchSet that is specified by @GeoMatchSetId@.
module Amazonka.WAFRegional.GetGeoMatchSet
  ( -- * Creating a Request
    GetGeoMatchSet (..),
    newGetGeoMatchSet,

    -- * Request Lenses
    getGeoMatchSet_geoMatchSetId,

    -- * Destructuring the Response
    GetGeoMatchSetResponse (..),
    newGetGeoMatchSetResponse,

    -- * Response Lenses
    getGeoMatchSetResponse_geoMatchSet,
    getGeoMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newGetGeoMatchSet' smart constructor.
data GetGeoMatchSet = GetGeoMatchSet'
  { -- | The @GeoMatchSetId@ of the GeoMatchSet that you want to get.
    -- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
    -- ListGeoMatchSets.
    geoMatchSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeoMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoMatchSetId', 'getGeoMatchSet_geoMatchSetId' - The @GeoMatchSetId@ of the GeoMatchSet that you want to get.
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
newGetGeoMatchSet ::
  -- | 'geoMatchSetId'
  Prelude.Text ->
  GetGeoMatchSet
newGetGeoMatchSet pGeoMatchSetId_ =
  GetGeoMatchSet' {geoMatchSetId = pGeoMatchSetId_}

-- | The @GeoMatchSetId@ of the GeoMatchSet that you want to get.
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
getGeoMatchSet_geoMatchSetId :: Lens.Lens' GetGeoMatchSet Prelude.Text
getGeoMatchSet_geoMatchSetId = Lens.lens (\GetGeoMatchSet' {geoMatchSetId} -> geoMatchSetId) (\s@GetGeoMatchSet' {} a -> s {geoMatchSetId = a} :: GetGeoMatchSet)

instance Core.AWSRequest GetGeoMatchSet where
  type
    AWSResponse GetGeoMatchSet =
      GetGeoMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGeoMatchSetResponse'
            Prelude.<$> (x Data..?> "GeoMatchSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGeoMatchSet where
  hashWithSalt _salt GetGeoMatchSet' {..} =
    _salt `Prelude.hashWithSalt` geoMatchSetId

instance Prelude.NFData GetGeoMatchSet where
  rnf GetGeoMatchSet' {..} = Prelude.rnf geoMatchSetId

instance Data.ToHeaders GetGeoMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.GetGeoMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetGeoMatchSet where
  toJSON GetGeoMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GeoMatchSetId" Data..= geoMatchSetId)
          ]
      )

instance Data.ToPath GetGeoMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetGeoMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGeoMatchSetResponse' smart constructor.
data GetGeoMatchSetResponse = GetGeoMatchSetResponse'
  { -- | Information about the GeoMatchSet that you specified in the
    -- @GetGeoMatchSet@ request. This includes the @Type@, which for a
    -- @GeoMatchContraint@ is always @Country@, as well as the @Value@, which
    -- is the identifier for a specific country.
    geoMatchSet :: Prelude.Maybe GeoMatchSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeoMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoMatchSet', 'getGeoMatchSetResponse_geoMatchSet' - Information about the GeoMatchSet that you specified in the
-- @GetGeoMatchSet@ request. This includes the @Type@, which for a
-- @GeoMatchContraint@ is always @Country@, as well as the @Value@, which
-- is the identifier for a specific country.
--
-- 'httpStatus', 'getGeoMatchSetResponse_httpStatus' - The response's http status code.
newGetGeoMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGeoMatchSetResponse
newGetGeoMatchSetResponse pHttpStatus_ =
  GetGeoMatchSetResponse'
    { geoMatchSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the GeoMatchSet that you specified in the
-- @GetGeoMatchSet@ request. This includes the @Type@, which for a
-- @GeoMatchContraint@ is always @Country@, as well as the @Value@, which
-- is the identifier for a specific country.
getGeoMatchSetResponse_geoMatchSet :: Lens.Lens' GetGeoMatchSetResponse (Prelude.Maybe GeoMatchSet)
getGeoMatchSetResponse_geoMatchSet = Lens.lens (\GetGeoMatchSetResponse' {geoMatchSet} -> geoMatchSet) (\s@GetGeoMatchSetResponse' {} a -> s {geoMatchSet = a} :: GetGeoMatchSetResponse)

-- | The response's http status code.
getGeoMatchSetResponse_httpStatus :: Lens.Lens' GetGeoMatchSetResponse Prelude.Int
getGeoMatchSetResponse_httpStatus = Lens.lens (\GetGeoMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetGeoMatchSetResponse' {} a -> s {httpStatus = a} :: GetGeoMatchSetResponse)

instance Prelude.NFData GetGeoMatchSetResponse where
  rnf GetGeoMatchSetResponse' {..} =
    Prelude.rnf geoMatchSet
      `Prelude.seq` Prelude.rnf httpStatus
