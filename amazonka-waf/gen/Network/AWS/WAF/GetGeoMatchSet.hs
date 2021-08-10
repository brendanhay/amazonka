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
-- Module      : Network.AWS.WAF.GetGeoMatchSet
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.WAF.GetGeoMatchSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGeoMatchSetResponse'
            Prelude.<$> (x Core..?> "GeoMatchSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGeoMatchSet

instance Prelude.NFData GetGeoMatchSet

instance Core.ToHeaders GetGeoMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetGeoMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetGeoMatchSet where
  toJSON GetGeoMatchSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GeoMatchSetId" Core..= geoMatchSetId)
          ]
      )

instance Core.ToPath GetGeoMatchSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetGeoMatchSet where
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

instance Prelude.NFData GetGeoMatchSetResponse
