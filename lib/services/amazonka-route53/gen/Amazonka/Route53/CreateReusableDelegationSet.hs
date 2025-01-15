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
-- Module      : Amazonka.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delegation set (a group of four name servers) that can be
-- reused by multiple hosted zones that were created by the same Amazon Web
-- Services account.
--
-- You can also create a reusable delegation set that uses the four name
-- servers that are associated with an existing hosted zone. Specify the
-- hosted zone ID in the @CreateReusableDelegationSet@ request.
--
-- You can\'t associate a reusable delegation set with a private hosted
-- zone.
--
-- For information about using a reusable delegation set to configure white
-- label name servers, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html Configuring White Label Name Servers>.
--
-- The process for migrating existing hosted zones to use a reusable
-- delegation set is comparable to the process for configuring white label
-- name servers. You need to perform the following steps:
--
-- 1.  Create a reusable delegation set.
--
-- 2.  Recreate hosted zones, and reduce the TTL to 60 seconds or less.
--
-- 3.  Recreate resource record sets in the new hosted zones.
--
-- 4.  Change the registrar\'s name servers to use the name servers for the
--     new hosted zones.
--
-- 5.  Monitor traffic for the website or application.
--
-- 6.  Change TTLs back to their original values.
--
-- If you want to migrate existing hosted zones to use a reusable
-- delegation set, the existing hosted zones can\'t use any of the name
-- servers that are assigned to the reusable delegation set. If one or more
-- hosted zones do use one or more name servers that are assigned to the
-- reusable delegation set, you can do one of the following:
--
-- -   For small numbers of hosted zones—up to a few hundred—it\'s
--     relatively easy to create reusable delegation sets until you get one
--     that has four name servers that don\'t overlap with any of the name
--     servers in your hosted zones.
--
-- -   For larger numbers of hosted zones, the easiest solution is to use
--     more than one reusable delegation set.
--
-- -   For larger numbers of hosted zones, you can also migrate hosted
--     zones that have overlapping name servers to hosted zones that don\'t
--     have overlapping name servers, then migrate the hosted zones again
--     to use the reusable delegation set.
module Amazonka.Route53.CreateReusableDelegationSet
  ( -- * Creating a Request
    CreateReusableDelegationSet (..),
    newCreateReusableDelegationSet,

    -- * Request Lenses
    createReusableDelegationSet_hostedZoneId,
    createReusableDelegationSet_callerReference,

    -- * Destructuring the Response
    CreateReusableDelegationSetResponse (..),
    newCreateReusableDelegationSetResponse,

    -- * Response Lenses
    createReusableDelegationSetResponse_httpStatus,
    createReusableDelegationSetResponse_delegationSet,
    createReusableDelegationSetResponse_location,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newCreateReusableDelegationSet' smart constructor.
data CreateReusableDelegationSet = CreateReusableDelegationSet'
  { -- | If you want to mark the delegation set for an existing hosted zone as
    -- reusable, the ID for that hosted zone.
    hostedZoneId :: Prelude.Maybe ResourceId,
    -- | A unique string that identifies the request, and that allows you to
    -- retry failed @CreateReusableDelegationSet@ requests without the risk of
    -- executing the operation twice. You must use a unique @CallerReference@
    -- string every time you submit a @CreateReusableDelegationSet@ request.
    -- @CallerReference@ can be any unique string, for example a date\/time
    -- stamp.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReusableDelegationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'createReusableDelegationSet_hostedZoneId' - If you want to mark the delegation set for an existing hosted zone as
-- reusable, the ID for that hosted zone.
--
-- 'callerReference', 'createReusableDelegationSet_callerReference' - A unique string that identifies the request, and that allows you to
-- retry failed @CreateReusableDelegationSet@ requests without the risk of
-- executing the operation twice. You must use a unique @CallerReference@
-- string every time you submit a @CreateReusableDelegationSet@ request.
-- @CallerReference@ can be any unique string, for example a date\/time
-- stamp.
newCreateReusableDelegationSet ::
  -- | 'callerReference'
  Prelude.Text ->
  CreateReusableDelegationSet
newCreateReusableDelegationSet pCallerReference_ =
  CreateReusableDelegationSet'
    { hostedZoneId =
        Prelude.Nothing,
      callerReference = pCallerReference_
    }

-- | If you want to mark the delegation set for an existing hosted zone as
-- reusable, the ID for that hosted zone.
createReusableDelegationSet_hostedZoneId :: Lens.Lens' CreateReusableDelegationSet (Prelude.Maybe ResourceId)
createReusableDelegationSet_hostedZoneId = Lens.lens (\CreateReusableDelegationSet' {hostedZoneId} -> hostedZoneId) (\s@CreateReusableDelegationSet' {} a -> s {hostedZoneId = a} :: CreateReusableDelegationSet)

-- | A unique string that identifies the request, and that allows you to
-- retry failed @CreateReusableDelegationSet@ requests without the risk of
-- executing the operation twice. You must use a unique @CallerReference@
-- string every time you submit a @CreateReusableDelegationSet@ request.
-- @CallerReference@ can be any unique string, for example a date\/time
-- stamp.
createReusableDelegationSet_callerReference :: Lens.Lens' CreateReusableDelegationSet Prelude.Text
createReusableDelegationSet_callerReference = Lens.lens (\CreateReusableDelegationSet' {callerReference} -> callerReference) (\s@CreateReusableDelegationSet' {} a -> s {callerReference = a} :: CreateReusableDelegationSet)

instance Core.AWSRequest CreateReusableDelegationSet where
  type
    AWSResponse CreateReusableDelegationSet =
      CreateReusableDelegationSetResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateReusableDelegationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "DelegationSet")
            Prelude.<*> (h Data..# "Location")
      )

instance Prelude.Hashable CreateReusableDelegationSet where
  hashWithSalt _salt CreateReusableDelegationSet' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` callerReference

instance Prelude.NFData CreateReusableDelegationSet where
  rnf CreateReusableDelegationSet' {..} =
    Prelude.rnf hostedZoneId `Prelude.seq`
      Prelude.rnf callerReference

instance Data.ToElement CreateReusableDelegationSet where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateReusableDelegationSetRequest"

instance Data.ToHeaders CreateReusableDelegationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateReusableDelegationSet where
  toPath = Prelude.const "/2013-04-01/delegationset"

instance Data.ToQuery CreateReusableDelegationSet where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateReusableDelegationSet where
  toXML CreateReusableDelegationSet' {..} =
    Prelude.mconcat
      [ "HostedZoneId" Data.@= hostedZoneId,
        "CallerReference" Data.@= callerReference
      ]

-- | /See:/ 'newCreateReusableDelegationSetResponse' smart constructor.
data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains name server information.
    delegationSet :: DelegationSet,
    -- | The unique URL representing the new reusable delegation set.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReusableDelegationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createReusableDelegationSetResponse_httpStatus' - The response's http status code.
--
-- 'delegationSet', 'createReusableDelegationSetResponse_delegationSet' - A complex type that contains name server information.
--
-- 'location', 'createReusableDelegationSetResponse_location' - The unique URL representing the new reusable delegation set.
newCreateReusableDelegationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'delegationSet'
  DelegationSet ->
  -- | 'location'
  Prelude.Text ->
  CreateReusableDelegationSetResponse
newCreateReusableDelegationSetResponse
  pHttpStatus_
  pDelegationSet_
  pLocation_ =
    CreateReusableDelegationSetResponse'
      { httpStatus =
          pHttpStatus_,
        delegationSet = pDelegationSet_,
        location = pLocation_
      }

-- | The response's http status code.
createReusableDelegationSetResponse_httpStatus :: Lens.Lens' CreateReusableDelegationSetResponse Prelude.Int
createReusableDelegationSetResponse_httpStatus = Lens.lens (\CreateReusableDelegationSetResponse' {httpStatus} -> httpStatus) (\s@CreateReusableDelegationSetResponse' {} a -> s {httpStatus = a} :: CreateReusableDelegationSetResponse)

-- | A complex type that contains name server information.
createReusableDelegationSetResponse_delegationSet :: Lens.Lens' CreateReusableDelegationSetResponse DelegationSet
createReusableDelegationSetResponse_delegationSet = Lens.lens (\CreateReusableDelegationSetResponse' {delegationSet} -> delegationSet) (\s@CreateReusableDelegationSetResponse' {} a -> s {delegationSet = a} :: CreateReusableDelegationSetResponse)

-- | The unique URL representing the new reusable delegation set.
createReusableDelegationSetResponse_location :: Lens.Lens' CreateReusableDelegationSetResponse Prelude.Text
createReusableDelegationSetResponse_location = Lens.lens (\CreateReusableDelegationSetResponse' {location} -> location) (\s@CreateReusableDelegationSetResponse' {} a -> s {location = a} :: CreateReusableDelegationSetResponse)

instance
  Prelude.NFData
    CreateReusableDelegationSetResponse
  where
  rnf CreateReusableDelegationSetResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf delegationSet `Prelude.seq`
        Prelude.rnf location
