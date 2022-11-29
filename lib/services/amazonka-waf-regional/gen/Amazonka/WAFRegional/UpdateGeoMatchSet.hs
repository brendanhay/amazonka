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
-- Module      : Amazonka.WAFRegional.UpdateGeoMatchSet
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
-- Inserts or deletes GeoMatchConstraint objects in an @GeoMatchSet@. For
-- each @GeoMatchConstraint@ object, you specify the following values:
--
-- -   Whether to insert or delete the object from the array. If you want
--     to change an @GeoMatchConstraint@ object, you delete the existing
--     object and add a new one.
--
-- -   The @Type@. The only valid value for @Type@ is @Country@.
--
-- -   The @Value@, which is a two character code for the country to add to
--     the @GeoMatchConstraint@ object. Valid codes are listed in
--     GeoMatchConstraint$Value.
--
-- To create and configure an @GeoMatchSet@, perform the following steps:
--
-- 1.  Submit a CreateGeoMatchSet request.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateGeoMatchSet request.
--
-- 3.  Submit an @UpdateGeoMatchSet@ request to specify the country that
--     you want AWS WAF to watch for.
--
-- When you update an @GeoMatchSet@, you specify the country that you want
-- to add and\/or the country that you want to delete. If you want to
-- change a country, you delete the existing country and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.UpdateGeoMatchSet
  ( -- * Creating a Request
    UpdateGeoMatchSet (..),
    newUpdateGeoMatchSet,

    -- * Request Lenses
    updateGeoMatchSet_geoMatchSetId,
    updateGeoMatchSet_changeToken,
    updateGeoMatchSet_updates,

    -- * Destructuring the Response
    UpdateGeoMatchSetResponse (..),
    newUpdateGeoMatchSetResponse,

    -- * Response Lenses
    updateGeoMatchSetResponse_changeToken,
    updateGeoMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newUpdateGeoMatchSet' smart constructor.
data UpdateGeoMatchSet = UpdateGeoMatchSet'
  { -- | The @GeoMatchSetId@ of the GeoMatchSet that you want to update.
    -- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
    -- ListGeoMatchSets.
    geoMatchSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text,
    -- | An array of @GeoMatchSetUpdate@ objects that you want to insert into or
    -- delete from an GeoMatchSet. For more information, see the applicable
    -- data types:
    --
    -- -   GeoMatchSetUpdate: Contains @Action@ and @GeoMatchConstraint@
    --
    -- -   GeoMatchConstraint: Contains @Type@ and @Value@
    --
    --     You can have only one @Type@ and @Value@ per @GeoMatchConstraint@.
    --     To add multiple countries, include multiple @GeoMatchSetUpdate@
    --     objects in your request.
    updates :: Prelude.NonEmpty GeoMatchSetUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGeoMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoMatchSetId', 'updateGeoMatchSet_geoMatchSetId' - The @GeoMatchSetId@ of the GeoMatchSet that you want to update.
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
--
-- 'changeToken', 'updateGeoMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateGeoMatchSet_updates' - An array of @GeoMatchSetUpdate@ objects that you want to insert into or
-- delete from an GeoMatchSet. For more information, see the applicable
-- data types:
--
-- -   GeoMatchSetUpdate: Contains @Action@ and @GeoMatchConstraint@
--
-- -   GeoMatchConstraint: Contains @Type@ and @Value@
--
--     You can have only one @Type@ and @Value@ per @GeoMatchConstraint@.
--     To add multiple countries, include multiple @GeoMatchSetUpdate@
--     objects in your request.
newUpdateGeoMatchSet ::
  -- | 'geoMatchSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty GeoMatchSetUpdate ->
  UpdateGeoMatchSet
newUpdateGeoMatchSet
  pGeoMatchSetId_
  pChangeToken_
  pUpdates_ =
    UpdateGeoMatchSet'
      { geoMatchSetId = pGeoMatchSetId_,
        changeToken = pChangeToken_,
        updates = Lens.coerced Lens.# pUpdates_
      }

-- | The @GeoMatchSetId@ of the GeoMatchSet that you want to update.
-- @GeoMatchSetId@ is returned by CreateGeoMatchSet and by
-- ListGeoMatchSets.
updateGeoMatchSet_geoMatchSetId :: Lens.Lens' UpdateGeoMatchSet Prelude.Text
updateGeoMatchSet_geoMatchSetId = Lens.lens (\UpdateGeoMatchSet' {geoMatchSetId} -> geoMatchSetId) (\s@UpdateGeoMatchSet' {} a -> s {geoMatchSetId = a} :: UpdateGeoMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
updateGeoMatchSet_changeToken :: Lens.Lens' UpdateGeoMatchSet Prelude.Text
updateGeoMatchSet_changeToken = Lens.lens (\UpdateGeoMatchSet' {changeToken} -> changeToken) (\s@UpdateGeoMatchSet' {} a -> s {changeToken = a} :: UpdateGeoMatchSet)

-- | An array of @GeoMatchSetUpdate@ objects that you want to insert into or
-- delete from an GeoMatchSet. For more information, see the applicable
-- data types:
--
-- -   GeoMatchSetUpdate: Contains @Action@ and @GeoMatchConstraint@
--
-- -   GeoMatchConstraint: Contains @Type@ and @Value@
--
--     You can have only one @Type@ and @Value@ per @GeoMatchConstraint@.
--     To add multiple countries, include multiple @GeoMatchSetUpdate@
--     objects in your request.
updateGeoMatchSet_updates :: Lens.Lens' UpdateGeoMatchSet (Prelude.NonEmpty GeoMatchSetUpdate)
updateGeoMatchSet_updates = Lens.lens (\UpdateGeoMatchSet' {updates} -> updates) (\s@UpdateGeoMatchSet' {} a -> s {updates = a} :: UpdateGeoMatchSet) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateGeoMatchSet where
  type
    AWSResponse UpdateGeoMatchSet =
      UpdateGeoMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGeoMatchSetResponse'
            Prelude.<$> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGeoMatchSet where
  hashWithSalt _salt UpdateGeoMatchSet' {..} =
    _salt `Prelude.hashWithSalt` geoMatchSetId
      `Prelude.hashWithSalt` changeToken
      `Prelude.hashWithSalt` updates

instance Prelude.NFData UpdateGeoMatchSet where
  rnf UpdateGeoMatchSet' {..} =
    Prelude.rnf geoMatchSetId
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf updates

instance Core.ToHeaders UpdateGeoMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.UpdateGeoMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGeoMatchSet where
  toJSON UpdateGeoMatchSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GeoMatchSetId" Core..= geoMatchSetId),
            Prelude.Just ("ChangeToken" Core..= changeToken),
            Prelude.Just ("Updates" Core..= updates)
          ]
      )

instance Core.ToPath UpdateGeoMatchSet where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGeoMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGeoMatchSetResponse' smart constructor.
data UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGeoMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateGeoMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateGeoMatchSetResponse_httpStatus' - The response's http status code.
newUpdateGeoMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGeoMatchSetResponse
newUpdateGeoMatchSetResponse pHttpStatus_ =
  UpdateGeoMatchSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateGeoMatchSetResponse_changeToken :: Lens.Lens' UpdateGeoMatchSetResponse (Prelude.Maybe Prelude.Text)
updateGeoMatchSetResponse_changeToken = Lens.lens (\UpdateGeoMatchSetResponse' {changeToken} -> changeToken) (\s@UpdateGeoMatchSetResponse' {} a -> s {changeToken = a} :: UpdateGeoMatchSetResponse)

-- | The response's http status code.
updateGeoMatchSetResponse_httpStatus :: Lens.Lens' UpdateGeoMatchSetResponse Prelude.Int
updateGeoMatchSetResponse_httpStatus = Lens.lens (\UpdateGeoMatchSetResponse' {httpStatus} -> httpStatus) (\s@UpdateGeoMatchSetResponse' {} a -> s {httpStatus = a} :: UpdateGeoMatchSetResponse)

instance Prelude.NFData UpdateGeoMatchSetResponse where
  rnf UpdateGeoMatchSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
