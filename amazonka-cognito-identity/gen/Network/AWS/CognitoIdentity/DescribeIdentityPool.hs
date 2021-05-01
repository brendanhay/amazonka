{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentityPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a particular identity pool, including the pool name,
-- ID description, creation date, and current number of users.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DescribeIdentityPool
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
    identityPool_samlProviderARNs,
    identityPool_identityPoolTags,
    identityPool_openIdConnectProviderARNs,
    identityPool_supportedLoginProviders,
    identityPool_cognitoIdentityProviders,
    identityPool_developerProviderName,
    identityPool_identityPoolId,
    identityPool_identityPoolName,
    identityPool_allowUnauthenticatedIdentities,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the DescribeIdentityPool action.
--
-- /See:/ 'newDescribeIdentityPool' smart constructor.
data DescribeIdentityPool = DescribeIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeIdentityPool where
  type Rs DescribeIdentityPool = IdentityPool
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable DescribeIdentityPool

instance Prelude.NFData DescribeIdentityPool

instance Prelude.ToHeaders DescribeIdentityPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityService.DescribeIdentityPool" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeIdentityPool where
  toJSON DescribeIdentityPool' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityPoolId" Prelude..= identityPoolId)
          ]
      )

instance Prelude.ToPath DescribeIdentityPool where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeIdentityPool where
  toQuery = Prelude.const Prelude.mempty
