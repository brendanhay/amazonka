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
-- Module      : Amazonka.CognitoIdentity.DescribeIdentityPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a particular identity pool, including the pool name,
-- ID description, creation date, and current number of users.
--
-- You must use AWS Developer credentials to call this API.
module Amazonka.CognitoIdentity.DescribeIdentityPool
  ( -- * Creating a Request
    DescribeIdentityPool (..),
    newDescribeIdentityPool,

    -- * Request Lenses
    describeIdentityPool_identityPoolId,

    -- * Destructuring the Response
    IdentityPool (..),
    newIdentityPool,

    -- * Response Lenses
    identityPool_allowClassicFlow,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_samlProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the DescribeIdentityPool action.
--
-- /See:/ 'newDescribeIdentityPool' smart constructor.
data DescribeIdentityPool = DescribeIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'describeIdentityPool_identityPoolId' - An identity pool ID in the format REGION:GUID.
newDescribeIdentityPool ::
  -- | 'identityPoolId'
  Prelude.Text ->
  DescribeIdentityPool
newDescribeIdentityPool pIdentityPoolId_ =
  DescribeIdentityPool'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | An identity pool ID in the format REGION:GUID.
describeIdentityPool_identityPoolId :: Lens.Lens' DescribeIdentityPool Prelude.Text
describeIdentityPool_identityPoolId = Lens.lens (\DescribeIdentityPool' {identityPoolId} -> identityPoolId) (\s@DescribeIdentityPool' {} a -> s {identityPoolId = a} :: DescribeIdentityPool)

instance Core.AWSRequest DescribeIdentityPool where
  type AWSResponse DescribeIdentityPool = IdentityPool
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeIdentityPool where
  hashWithSalt _salt DescribeIdentityPool' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData DescribeIdentityPool where
  rnf DescribeIdentityPool' {..} =
    Prelude.rnf identityPoolId

instance Data.ToHeaders DescribeIdentityPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.DescribeIdentityPool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeIdentityPool where
  toJSON DescribeIdentityPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityPoolId" Data..= identityPoolId)
          ]
      )

instance Data.ToPath DescribeIdentityPool where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIdentityPool where
  toQuery = Prelude.const Prelude.mempty
