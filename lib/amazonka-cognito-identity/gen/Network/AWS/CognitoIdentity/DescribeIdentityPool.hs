{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a particular identity pool, including the pool name, ID description, creation date, and current number of users.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DescribeIdentityPool
  ( -- * Creating a request
    DescribeIdentityPool (..),
    mkDescribeIdentityPool,

    -- ** Request lenses
    dIdentityPoolId,

    -- * Destructuring the response
    IdentityPool (..),
    mkIdentityPool,

    -- ** Response lenses
    ipSamlProviderARNs,
    ipSupportedLoginProviders,
    ipIdentityPoolId,
    ipAllowClassicFlow,
    ipIdentityPoolName,
    ipDeveloperProviderName,
    ipIdentityPoolTags,
    ipOpenIdConnectProviderARNs,
    ipCognitoIdentityProviders,
    ipAllowUnauthenticatedIdentities,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the DescribeIdentityPool action.
--
-- /See:/ 'mkDescribeIdentityPool' smart constructor.
newtype DescribeIdentityPool = DescribeIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentityPool' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
mkDescribeIdentityPool ::
  -- | 'identityPoolId'
  Lude.Text ->
  DescribeIdentityPool
mkDescribeIdentityPool pIdentityPoolId_ =
  DescribeIdentityPool' {identityPoolId = pIdentityPoolId_}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIdentityPoolId :: Lens.Lens' DescribeIdentityPool Lude.Text
dIdentityPoolId = Lens.lens (identityPoolId :: DescribeIdentityPool -> Lude.Text) (\s a -> s {identityPoolId = a} :: DescribeIdentityPool)
{-# DEPRECATED dIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest DescribeIdentityPool where
  type Rs DescribeIdentityPool = IdentityPool
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeIdentityPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.DescribeIdentityPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeIdentityPool where
  toJSON DescribeIdentityPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("IdentityPoolId" Lude..= identityPoolId)]
      )

instance Lude.ToPath DescribeIdentityPool where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIdentityPool where
  toQuery = Lude.const Lude.mempty
