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
-- Module      : Amazonka.WAFRegional.DeleteGeoMatchSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- Permanently deletes a GeoMatchSet. You can\'t delete a @GeoMatchSet@ if
-- it\'s still used in any @Rules@ or if it still includes any countries.
--
-- If you just want to remove a @GeoMatchSet@ from a @Rule@, use
-- UpdateRule.
--
-- To permanently delete a @GeoMatchSet@ from AWS WAF, perform the
-- following steps:
--
-- 1.  Update the @GeoMatchSet@ to remove any countries. For more
--     information, see UpdateGeoMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteGeoMatchSet@ request.
--
-- 3.  Submit a @DeleteGeoMatchSet@ request.
module Amazonka.WAFRegional.DeleteGeoMatchSet
  ( -- * Creating a Request
    DeleteGeoMatchSet (..),
    newDeleteGeoMatchSet,

    -- * Request Lenses
    deleteGeoMatchSet_geoMatchSetId,
    deleteGeoMatchSet_changeToken,

    -- * Destructuring the Response
    DeleteGeoMatchSetResponse (..),
    newDeleteGeoMatchSetResponse,

    -- * Response Lenses
    deleteGeoMatchSetResponse_changeToken,
    deleteGeoMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newDeleteGeoMatchSet' smart constructor.
data DeleteGeoMatchSet = DeleteGeoMatchSet'
  { -- | The @GeoMatchSetID@ of the GeoMatchSet that you want to delete.
    -- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
    -- ListGeoMatchSets.
    geoMatchSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGeoMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoMatchSetId', 'deleteGeoMatchSet_geoMatchSetId' - The @GeoMatchSetID@ of the GeoMatchSet that you want to delete.
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
--
-- 'changeToken', 'deleteGeoMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteGeoMatchSet ::
  -- | 'geoMatchSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteGeoMatchSet
newDeleteGeoMatchSet pGeoMatchSetId_ pChangeToken_ =
  DeleteGeoMatchSet'
    { geoMatchSetId = pGeoMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @GeoMatchSetID@ of the GeoMatchSet that you want to delete.
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
deleteGeoMatchSet_geoMatchSetId :: Lens.Lens' DeleteGeoMatchSet Prelude.Text
deleteGeoMatchSet_geoMatchSetId = Lens.lens (\DeleteGeoMatchSet' {geoMatchSetId} -> geoMatchSetId) (\s@DeleteGeoMatchSet' {} a -> s {geoMatchSetId = a} :: DeleteGeoMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteGeoMatchSet_changeToken :: Lens.Lens' DeleteGeoMatchSet Prelude.Text
deleteGeoMatchSet_changeToken = Lens.lens (\DeleteGeoMatchSet' {changeToken} -> changeToken) (\s@DeleteGeoMatchSet' {} a -> s {changeToken = a} :: DeleteGeoMatchSet)

instance Core.AWSRequest DeleteGeoMatchSet where
  type
    AWSResponse DeleteGeoMatchSet =
      DeleteGeoMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGeoMatchSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGeoMatchSet where
  hashWithSalt _salt DeleteGeoMatchSet' {..} =
    _salt
      `Prelude.hashWithSalt` geoMatchSetId
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData DeleteGeoMatchSet where
  rnf DeleteGeoMatchSet' {..} =
    Prelude.rnf geoMatchSetId
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders DeleteGeoMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.DeleteGeoMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGeoMatchSet where
  toJSON DeleteGeoMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GeoMatchSetId" Data..= geoMatchSetId),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath DeleteGeoMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGeoMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGeoMatchSetResponse' smart constructor.
data DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGeoMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteGeoMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteGeoMatchSetResponse_httpStatus' - The response's http status code.
newDeleteGeoMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGeoMatchSetResponse
newDeleteGeoMatchSetResponse pHttpStatus_ =
  DeleteGeoMatchSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteGeoMatchSetResponse_changeToken :: Lens.Lens' DeleteGeoMatchSetResponse (Prelude.Maybe Prelude.Text)
deleteGeoMatchSetResponse_changeToken = Lens.lens (\DeleteGeoMatchSetResponse' {changeToken} -> changeToken) (\s@DeleteGeoMatchSetResponse' {} a -> s {changeToken = a} :: DeleteGeoMatchSetResponse)

-- | The response's http status code.
deleteGeoMatchSetResponse_httpStatus :: Lens.Lens' DeleteGeoMatchSetResponse Prelude.Int
deleteGeoMatchSetResponse_httpStatus = Lens.lens (\DeleteGeoMatchSetResponse' {httpStatus} -> httpStatus) (\s@DeleteGeoMatchSetResponse' {} a -> s {httpStatus = a} :: DeleteGeoMatchSetResponse)

instance Prelude.NFData DeleteGeoMatchSetResponse where
  rnf DeleteGeoMatchSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
