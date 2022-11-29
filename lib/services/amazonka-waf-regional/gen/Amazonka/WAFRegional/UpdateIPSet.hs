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
-- Module      : Amazonka.WAFRegional.UpdateIPSet
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
-- Inserts or deletes IPSetDescriptor objects in an @IPSet@. For each
-- @IPSetDescriptor@ object, you specify the following values:
--
-- -   Whether to insert or delete the object from the array. If you want
--     to change an @IPSetDescriptor@ object, you delete the existing
--     object and add a new one.
--
-- -   The IP address version, @IPv4@ or @IPv6@.
--
-- -   The IP address in CIDR notation, for example, @192.0.2.0\/24@ (for
--     the range of IP addresses from @192.0.2.0@ to @192.0.2.255@) or
--     @192.0.2.44\/32@ (for the individual IP address @192.0.2.44@).
--
-- AWS WAF supports IPv4 address ranges: \/8 and any range between \/16
-- through \/32. AWS WAF supports IPv6 address ranges: \/24, \/32, \/48,
-- \/56, \/64, and \/128. For more information about CIDR notation, see the
-- Wikipedia entry
-- <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing>.
--
-- IPv6 addresses can be represented using any of the following formats:
--
-- -   1111:0000:0000:0000:0000:0000:0000:0111\/128
--
-- -   1111:0:0:0:0:0:0:0111\/128
--
-- -   1111::0111\/128
--
-- -   1111::111\/128
--
-- You use an @IPSet@ to specify which web requests you want to allow or
-- block based on the IP addresses that the requests originated from. For
-- example, if you\'re receiving a lot of requests from one or a small
-- number of IP addresses and you want to block the requests, you can
-- create an @IPSet@ that specifies those IP addresses, and then configure
-- AWS WAF to block the requests.
--
-- To create and configure an @IPSet@, perform the following steps:
--
-- 1.  Submit a CreateIPSet request.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateIPSet request.
--
-- 3.  Submit an @UpdateIPSet@ request to specify the IP addresses that you
--     want AWS WAF to watch for.
--
-- When you update an @IPSet@, you specify the IP addresses that you want
-- to add and\/or the IP addresses that you want to delete. If you want to
-- change an IP address, you delete the existing IP address and add the new
-- one.
--
-- You can insert a maximum of 1000 addresses in a single request.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.UpdateIPSet
  ( -- * Creating a Request
    UpdateIPSet (..),
    newUpdateIPSet,

    -- * Request Lenses
    updateIPSet_iPSetId,
    updateIPSet_changeToken,
    updateIPSet_updates,

    -- * Destructuring the Response
    UpdateIPSetResponse (..),
    newUpdateIPSetResponse,

    -- * Response Lenses
    updateIPSetResponse_changeToken,
    updateIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { -- | The @IPSetId@ of the IPSet that you want to update. @IPSetId@ is
    -- returned by CreateIPSet and by ListIPSets.
    iPSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text,
    -- | An array of @IPSetUpdate@ objects that you want to insert into or delete
    -- from an IPSet. For more information, see the applicable data types:
    --
    -- -   IPSetUpdate: Contains @Action@ and @IPSetDescriptor@
    --
    -- -   IPSetDescriptor: Contains @Type@ and @Value@
    --
    -- You can insert a maximum of 1000 addresses in a single request.
    updates :: Prelude.NonEmpty IPSetUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSetId', 'updateIPSet_iPSetId' - The @IPSetId@ of the IPSet that you want to update. @IPSetId@ is
-- returned by CreateIPSet and by ListIPSets.
--
-- 'changeToken', 'updateIPSet_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateIPSet_updates' - An array of @IPSetUpdate@ objects that you want to insert into or delete
-- from an IPSet. For more information, see the applicable data types:
--
-- -   IPSetUpdate: Contains @Action@ and @IPSetDescriptor@
--
-- -   IPSetDescriptor: Contains @Type@ and @Value@
--
-- You can insert a maximum of 1000 addresses in a single request.
newUpdateIPSet ::
  -- | 'iPSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty IPSetUpdate ->
  UpdateIPSet
newUpdateIPSet pIPSetId_ pChangeToken_ pUpdates_ =
  UpdateIPSet'
    { iPSetId = pIPSetId_,
      changeToken = pChangeToken_,
      updates = Lens.coerced Lens.# pUpdates_
    }

-- | The @IPSetId@ of the IPSet that you want to update. @IPSetId@ is
-- returned by CreateIPSet and by ListIPSets.
updateIPSet_iPSetId :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_iPSetId = Lens.lens (\UpdateIPSet' {iPSetId} -> iPSetId) (\s@UpdateIPSet' {} a -> s {iPSetId = a} :: UpdateIPSet)

-- | The value returned by the most recent call to GetChangeToken.
updateIPSet_changeToken :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_changeToken = Lens.lens (\UpdateIPSet' {changeToken} -> changeToken) (\s@UpdateIPSet' {} a -> s {changeToken = a} :: UpdateIPSet)

-- | An array of @IPSetUpdate@ objects that you want to insert into or delete
-- from an IPSet. For more information, see the applicable data types:
--
-- -   IPSetUpdate: Contains @Action@ and @IPSetDescriptor@
--
-- -   IPSetDescriptor: Contains @Type@ and @Value@
--
-- You can insert a maximum of 1000 addresses in a single request.
updateIPSet_updates :: Lens.Lens' UpdateIPSet (Prelude.NonEmpty IPSetUpdate)
updateIPSet_updates = Lens.lens (\UpdateIPSet' {updates} -> updates) (\s@UpdateIPSet' {} a -> s {updates = a} :: UpdateIPSet) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateIPSet where
  type AWSResponse UpdateIPSet = UpdateIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIPSetResponse'
            Prelude.<$> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIPSet where
  hashWithSalt _salt UpdateIPSet' {..} =
    _salt `Prelude.hashWithSalt` iPSetId
      `Prelude.hashWithSalt` changeToken
      `Prelude.hashWithSalt` updates

instance Prelude.NFData UpdateIPSet where
  rnf UpdateIPSet' {..} =
    Prelude.rnf iPSetId
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf updates

instance Core.ToHeaders UpdateIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.UpdateIPSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateIPSet where
  toJSON UpdateIPSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IPSetId" Core..= iPSetId),
            Prelude.Just ("ChangeToken" Core..= changeToken),
            Prelude.Just ("Updates" Core..= updates)
          ]
      )

instance Core.ToPath UpdateIPSet where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateIPSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateIPSetResponse_httpStatus' - The response's http status code.
newUpdateIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIPSetResponse
newUpdateIPSetResponse pHttpStatus_ =
  UpdateIPSetResponse'
    { changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
updateIPSetResponse_changeToken :: Lens.Lens' UpdateIPSetResponse (Prelude.Maybe Prelude.Text)
updateIPSetResponse_changeToken = Lens.lens (\UpdateIPSetResponse' {changeToken} -> changeToken) (\s@UpdateIPSetResponse' {} a -> s {changeToken = a} :: UpdateIPSetResponse)

-- | The response's http status code.
updateIPSetResponse_httpStatus :: Lens.Lens' UpdateIPSetResponse Prelude.Int
updateIPSetResponse_httpStatus = Lens.lens (\UpdateIPSetResponse' {httpStatus} -> httpStatus) (\s@UpdateIPSetResponse' {} a -> s {httpStatus = a} :: UpdateIPSetResponse)

instance Prelude.NFData UpdateIPSetResponse where
  rnf UpdateIPSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
