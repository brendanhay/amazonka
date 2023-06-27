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
-- Module      : Amazonka.TNB.InstantiateSolNetworkInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instantiates a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- Before you can instantiate a network instance, you have to create a
-- network instance. For more information, see
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_CreateSolNetworkInstance.html CreateSolNetworkInstance>.
module Amazonka.TNB.InstantiateSolNetworkInstance
  ( -- * Creating a Request
    InstantiateSolNetworkInstance (..),
    newInstantiateSolNetworkInstance,

    -- * Request Lenses
    instantiateSolNetworkInstance_additionalParamsForNs,
    instantiateSolNetworkInstance_dryRun,
    instantiateSolNetworkInstance_tags,
    instantiateSolNetworkInstance_nsInstanceId,

    -- * Destructuring the Response
    InstantiateSolNetworkInstanceResponse (..),
    newInstantiateSolNetworkInstanceResponse,

    -- * Response Lenses
    instantiateSolNetworkInstanceResponse_tags,
    instantiateSolNetworkInstanceResponse_httpStatus,
    instantiateSolNetworkInstanceResponse_nsLcmOpOccId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newInstantiateSolNetworkInstance' smart constructor.
data InstantiateSolNetworkInstance = InstantiateSolNetworkInstance'
  { -- | Provides values for the configurable properties.
    additionalParamsForNs :: Prelude.Maybe Document,
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. When you use this API,
    -- the tags are transferred to the network operation that is created. Use
    -- tags to search and filter your resources or track your Amazon Web
    -- Services costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | ID of the network instance.
    nsInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstantiateSolNetworkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalParamsForNs', 'instantiateSolNetworkInstance_additionalParamsForNs' - Provides values for the configurable properties.
--
-- 'dryRun', 'instantiateSolNetworkInstance_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'tags', 'instantiateSolNetworkInstance_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
--
-- 'nsInstanceId', 'instantiateSolNetworkInstance_nsInstanceId' - ID of the network instance.
newInstantiateSolNetworkInstance ::
  -- | 'nsInstanceId'
  Prelude.Text ->
  InstantiateSolNetworkInstance
newInstantiateSolNetworkInstance pNsInstanceId_ =
  InstantiateSolNetworkInstance'
    { additionalParamsForNs =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      tags = Prelude.Nothing,
      nsInstanceId = pNsInstanceId_
    }

-- | Provides values for the configurable properties.
instantiateSolNetworkInstance_additionalParamsForNs :: Lens.Lens' InstantiateSolNetworkInstance (Prelude.Maybe Document)
instantiateSolNetworkInstance_additionalParamsForNs = Lens.lens (\InstantiateSolNetworkInstance' {additionalParamsForNs} -> additionalParamsForNs) (\s@InstantiateSolNetworkInstance' {} a -> s {additionalParamsForNs = a} :: InstantiateSolNetworkInstance)

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
instantiateSolNetworkInstance_dryRun :: Lens.Lens' InstantiateSolNetworkInstance (Prelude.Maybe Prelude.Bool)
instantiateSolNetworkInstance_dryRun = Lens.lens (\InstantiateSolNetworkInstance' {dryRun} -> dryRun) (\s@InstantiateSolNetworkInstance' {} a -> s {dryRun = a} :: InstantiateSolNetworkInstance)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
instantiateSolNetworkInstance_tags :: Lens.Lens' InstantiateSolNetworkInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
instantiateSolNetworkInstance_tags = Lens.lens (\InstantiateSolNetworkInstance' {tags} -> tags) (\s@InstantiateSolNetworkInstance' {} a -> s {tags = a} :: InstantiateSolNetworkInstance) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | ID of the network instance.
instantiateSolNetworkInstance_nsInstanceId :: Lens.Lens' InstantiateSolNetworkInstance Prelude.Text
instantiateSolNetworkInstance_nsInstanceId = Lens.lens (\InstantiateSolNetworkInstance' {nsInstanceId} -> nsInstanceId) (\s@InstantiateSolNetworkInstance' {} a -> s {nsInstanceId = a} :: InstantiateSolNetworkInstance)

instance
  Core.AWSRequest
    InstantiateSolNetworkInstance
  where
  type
    AWSResponse InstantiateSolNetworkInstance =
      InstantiateSolNetworkInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InstantiateSolNetworkInstanceResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "nsLcmOpOccId")
      )

instance
  Prelude.Hashable
    InstantiateSolNetworkInstance
  where
  hashWithSalt _salt InstantiateSolNetworkInstance' {..} =
    _salt
      `Prelude.hashWithSalt` additionalParamsForNs
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nsInstanceId

instance Prelude.NFData InstantiateSolNetworkInstance where
  rnf InstantiateSolNetworkInstance' {..} =
    Prelude.rnf additionalParamsForNs
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nsInstanceId

instance Data.ToHeaders InstantiateSolNetworkInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InstantiateSolNetworkInstance where
  toJSON InstantiateSolNetworkInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalParamsForNs" Data..=)
              Prelude.<$> additionalParamsForNs,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath InstantiateSolNetworkInstance where
  toPath InstantiateSolNetworkInstance' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_instances/",
        Data.toBS nsInstanceId,
        "/instantiate"
      ]

instance Data.ToQuery InstantiateSolNetworkInstance where
  toQuery InstantiateSolNetworkInstance' {..} =
    Prelude.mconcat ["dry_run" Data.=: dryRun]

-- | /See:/ 'newInstantiateSolNetworkInstanceResponse' smart constructor.
data InstantiateSolNetworkInstanceResponse = InstantiateSolNetworkInstanceResponse'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. When you use this API,
    -- the tags are transferred to the network operation that is created. Use
    -- tags to search and filter your resources or track your Amazon Web
    -- Services costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the network operation.
    nsLcmOpOccId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstantiateSolNetworkInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'instantiateSolNetworkInstanceResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
--
-- 'httpStatus', 'instantiateSolNetworkInstanceResponse_httpStatus' - The response's http status code.
--
-- 'nsLcmOpOccId', 'instantiateSolNetworkInstanceResponse_nsLcmOpOccId' - The identifier of the network operation.
newInstantiateSolNetworkInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nsLcmOpOccId'
  Prelude.Text ->
  InstantiateSolNetworkInstanceResponse
newInstantiateSolNetworkInstanceResponse
  pHttpStatus_
  pNsLcmOpOccId_ =
    InstantiateSolNetworkInstanceResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        nsLcmOpOccId = pNsLcmOpOccId_
      }

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
instantiateSolNetworkInstanceResponse_tags :: Lens.Lens' InstantiateSolNetworkInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
instantiateSolNetworkInstanceResponse_tags = Lens.lens (\InstantiateSolNetworkInstanceResponse' {tags} -> tags) (\s@InstantiateSolNetworkInstanceResponse' {} a -> s {tags = a} :: InstantiateSolNetworkInstanceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
instantiateSolNetworkInstanceResponse_httpStatus :: Lens.Lens' InstantiateSolNetworkInstanceResponse Prelude.Int
instantiateSolNetworkInstanceResponse_httpStatus = Lens.lens (\InstantiateSolNetworkInstanceResponse' {httpStatus} -> httpStatus) (\s@InstantiateSolNetworkInstanceResponse' {} a -> s {httpStatus = a} :: InstantiateSolNetworkInstanceResponse)

-- | The identifier of the network operation.
instantiateSolNetworkInstanceResponse_nsLcmOpOccId :: Lens.Lens' InstantiateSolNetworkInstanceResponse Prelude.Text
instantiateSolNetworkInstanceResponse_nsLcmOpOccId = Lens.lens (\InstantiateSolNetworkInstanceResponse' {nsLcmOpOccId} -> nsLcmOpOccId) (\s@InstantiateSolNetworkInstanceResponse' {} a -> s {nsLcmOpOccId = a} :: InstantiateSolNetworkInstanceResponse)

instance
  Prelude.NFData
    InstantiateSolNetworkInstanceResponse
  where
  rnf InstantiateSolNetworkInstanceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nsLcmOpOccId
