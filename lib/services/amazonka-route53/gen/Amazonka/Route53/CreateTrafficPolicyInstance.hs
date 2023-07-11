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
-- Module      : Amazonka.Route53.CreateTrafficPolicyInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates resource record sets in a specified hosted zone based on the
-- settings in a specified traffic policy version. In addition,
-- @CreateTrafficPolicyInstance@ associates the resource record sets with a
-- specified domain name (such as example.com) or subdomain name (such as
-- www.example.com). Amazon Route 53 responds to DNS queries for the domain
-- or subdomain name by using the resource record sets that
-- @CreateTrafficPolicyInstance@ created.
module Amazonka.Route53.CreateTrafficPolicyInstance
  ( -- * Creating a Request
    CreateTrafficPolicyInstance (..),
    newCreateTrafficPolicyInstance,

    -- * Request Lenses
    createTrafficPolicyInstance_hostedZoneId,
    createTrafficPolicyInstance_name,
    createTrafficPolicyInstance_ttl,
    createTrafficPolicyInstance_trafficPolicyId,
    createTrafficPolicyInstance_trafficPolicyVersion,

    -- * Destructuring the Response
    CreateTrafficPolicyInstanceResponse (..),
    newCreateTrafficPolicyInstanceResponse,

    -- * Response Lenses
    createTrafficPolicyInstanceResponse_httpStatus,
    createTrafficPolicyInstanceResponse_trafficPolicyInstance,
    createTrafficPolicyInstanceResponse_location,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the resource record sets
-- that you want to create based on a specified traffic policy.
--
-- /See:/ 'newCreateTrafficPolicyInstance' smart constructor.
data CreateTrafficPolicyInstance = CreateTrafficPolicyInstance'
  { -- | The ID of the hosted zone that you want Amazon Route 53 to create
    -- resource record sets in by using the configuration in a traffic policy.
    hostedZoneId :: ResourceId,
    -- | The domain name (such as example.com) or subdomain name (such as
    -- www.example.com) for which Amazon Route 53 responds to DNS queries by
    -- using the resource record sets that Route 53 creates for this traffic
    -- policy instance.
    name :: Prelude.Text,
    -- | (Optional) The TTL that you want Amazon Route 53 to assign to all of the
    -- resource record sets that it creates in the specified hosted zone.
    ttl :: Prelude.Natural,
    -- | The ID of the traffic policy that you want to use to create resource
    -- record sets in the specified hosted zone.
    trafficPolicyId :: Prelude.Text,
    -- | The version of the traffic policy that you want to use to create
    -- resource record sets in the specified hosted zone.
    trafficPolicyVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficPolicyInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'createTrafficPolicyInstance_hostedZoneId' - The ID of the hosted zone that you want Amazon Route 53 to create
-- resource record sets in by using the configuration in a traffic policy.
--
-- 'name', 'createTrafficPolicyInstance_name' - The domain name (such as example.com) or subdomain name (such as
-- www.example.com) for which Amazon Route 53 responds to DNS queries by
-- using the resource record sets that Route 53 creates for this traffic
-- policy instance.
--
-- 'ttl', 'createTrafficPolicyInstance_ttl' - (Optional) The TTL that you want Amazon Route 53 to assign to all of the
-- resource record sets that it creates in the specified hosted zone.
--
-- 'trafficPolicyId', 'createTrafficPolicyInstance_trafficPolicyId' - The ID of the traffic policy that you want to use to create resource
-- record sets in the specified hosted zone.
--
-- 'trafficPolicyVersion', 'createTrafficPolicyInstance_trafficPolicyVersion' - The version of the traffic policy that you want to use to create
-- resource record sets in the specified hosted zone.
newCreateTrafficPolicyInstance ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  -- | 'ttl'
  Prelude.Natural ->
  -- | 'trafficPolicyId'
  Prelude.Text ->
  -- | 'trafficPolicyVersion'
  Prelude.Natural ->
  CreateTrafficPolicyInstance
newCreateTrafficPolicyInstance
  pHostedZoneId_
  pName_
  pTTL_
  pTrafficPolicyId_
  pTrafficPolicyVersion_ =
    CreateTrafficPolicyInstance'
      { hostedZoneId =
          pHostedZoneId_,
        name = pName_,
        ttl = pTTL_,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion = pTrafficPolicyVersion_
      }

-- | The ID of the hosted zone that you want Amazon Route 53 to create
-- resource record sets in by using the configuration in a traffic policy.
createTrafficPolicyInstance_hostedZoneId :: Lens.Lens' CreateTrafficPolicyInstance ResourceId
createTrafficPolicyInstance_hostedZoneId = Lens.lens (\CreateTrafficPolicyInstance' {hostedZoneId} -> hostedZoneId) (\s@CreateTrafficPolicyInstance' {} a -> s {hostedZoneId = a} :: CreateTrafficPolicyInstance)

-- | The domain name (such as example.com) or subdomain name (such as
-- www.example.com) for which Amazon Route 53 responds to DNS queries by
-- using the resource record sets that Route 53 creates for this traffic
-- policy instance.
createTrafficPolicyInstance_name :: Lens.Lens' CreateTrafficPolicyInstance Prelude.Text
createTrafficPolicyInstance_name = Lens.lens (\CreateTrafficPolicyInstance' {name} -> name) (\s@CreateTrafficPolicyInstance' {} a -> s {name = a} :: CreateTrafficPolicyInstance)

-- | (Optional) The TTL that you want Amazon Route 53 to assign to all of the
-- resource record sets that it creates in the specified hosted zone.
createTrafficPolicyInstance_ttl :: Lens.Lens' CreateTrafficPolicyInstance Prelude.Natural
createTrafficPolicyInstance_ttl = Lens.lens (\CreateTrafficPolicyInstance' {ttl} -> ttl) (\s@CreateTrafficPolicyInstance' {} a -> s {ttl = a} :: CreateTrafficPolicyInstance)

-- | The ID of the traffic policy that you want to use to create resource
-- record sets in the specified hosted zone.
createTrafficPolicyInstance_trafficPolicyId :: Lens.Lens' CreateTrafficPolicyInstance Prelude.Text
createTrafficPolicyInstance_trafficPolicyId = Lens.lens (\CreateTrafficPolicyInstance' {trafficPolicyId} -> trafficPolicyId) (\s@CreateTrafficPolicyInstance' {} a -> s {trafficPolicyId = a} :: CreateTrafficPolicyInstance)

-- | The version of the traffic policy that you want to use to create
-- resource record sets in the specified hosted zone.
createTrafficPolicyInstance_trafficPolicyVersion :: Lens.Lens' CreateTrafficPolicyInstance Prelude.Natural
createTrafficPolicyInstance_trafficPolicyVersion = Lens.lens (\CreateTrafficPolicyInstance' {trafficPolicyVersion} -> trafficPolicyVersion) (\s@CreateTrafficPolicyInstance' {} a -> s {trafficPolicyVersion = a} :: CreateTrafficPolicyInstance)

instance Core.AWSRequest CreateTrafficPolicyInstance where
  type
    AWSResponse CreateTrafficPolicyInstance =
      CreateTrafficPolicyInstanceResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficPolicyInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "TrafficPolicyInstance")
            Prelude.<*> (h Data..# "Location")
      )

instance Prelude.Hashable CreateTrafficPolicyInstance where
  hashWithSalt _salt CreateTrafficPolicyInstance' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` trafficPolicyId
      `Prelude.hashWithSalt` trafficPolicyVersion

instance Prelude.NFData CreateTrafficPolicyInstance where
  rnf CreateTrafficPolicyInstance' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf trafficPolicyId
      `Prelude.seq` Prelude.rnf trafficPolicyVersion

instance Data.ToElement CreateTrafficPolicyInstance where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateTrafficPolicyInstanceRequest"

instance Data.ToHeaders CreateTrafficPolicyInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTrafficPolicyInstance where
  toPath =
    Prelude.const "/2013-04-01/trafficpolicyinstance"

instance Data.ToQuery CreateTrafficPolicyInstance where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateTrafficPolicyInstance where
  toXML CreateTrafficPolicyInstance' {..} =
    Prelude.mconcat
      [ "HostedZoneId" Data.@= hostedZoneId,
        "Name" Data.@= name,
        "TTL" Data.@= ttl,
        "TrafficPolicyId" Data.@= trafficPolicyId,
        "TrafficPolicyVersion" Data.@= trafficPolicyVersion
      ]

-- | A complex type that contains the response information for the
-- @CreateTrafficPolicyInstance@ request.
--
-- /See:/ 'newCreateTrafficPolicyInstanceResponse' smart constructor.
data CreateTrafficPolicyInstanceResponse = CreateTrafficPolicyInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains settings for the new traffic policy
    -- instance.
    trafficPolicyInstance :: TrafficPolicyInstance,
    -- | A unique URL that represents a new traffic policy instance.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficPolicyInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTrafficPolicyInstanceResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicyInstance', 'createTrafficPolicyInstanceResponse_trafficPolicyInstance' - A complex type that contains settings for the new traffic policy
-- instance.
--
-- 'location', 'createTrafficPolicyInstanceResponse_location' - A unique URL that represents a new traffic policy instance.
newCreateTrafficPolicyInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trafficPolicyInstance'
  TrafficPolicyInstance ->
  -- | 'location'
  Prelude.Text ->
  CreateTrafficPolicyInstanceResponse
newCreateTrafficPolicyInstanceResponse
  pHttpStatus_
  pTrafficPolicyInstance_
  pLocation_ =
    CreateTrafficPolicyInstanceResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicyInstance =
          pTrafficPolicyInstance_,
        location = pLocation_
      }

-- | The response's http status code.
createTrafficPolicyInstanceResponse_httpStatus :: Lens.Lens' CreateTrafficPolicyInstanceResponse Prelude.Int
createTrafficPolicyInstanceResponse_httpStatus = Lens.lens (\CreateTrafficPolicyInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficPolicyInstanceResponse' {} a -> s {httpStatus = a} :: CreateTrafficPolicyInstanceResponse)

-- | A complex type that contains settings for the new traffic policy
-- instance.
createTrafficPolicyInstanceResponse_trafficPolicyInstance :: Lens.Lens' CreateTrafficPolicyInstanceResponse TrafficPolicyInstance
createTrafficPolicyInstanceResponse_trafficPolicyInstance = Lens.lens (\CreateTrafficPolicyInstanceResponse' {trafficPolicyInstance} -> trafficPolicyInstance) (\s@CreateTrafficPolicyInstanceResponse' {} a -> s {trafficPolicyInstance = a} :: CreateTrafficPolicyInstanceResponse)

-- | A unique URL that represents a new traffic policy instance.
createTrafficPolicyInstanceResponse_location :: Lens.Lens' CreateTrafficPolicyInstanceResponse Prelude.Text
createTrafficPolicyInstanceResponse_location = Lens.lens (\CreateTrafficPolicyInstanceResponse' {location} -> location) (\s@CreateTrafficPolicyInstanceResponse' {} a -> s {location = a} :: CreateTrafficPolicyInstanceResponse)

instance
  Prelude.NFData
    CreateTrafficPolicyInstanceResponse
  where
  rnf CreateTrafficPolicyInstanceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicyInstance
      `Prelude.seq` Prelude.rnf location
