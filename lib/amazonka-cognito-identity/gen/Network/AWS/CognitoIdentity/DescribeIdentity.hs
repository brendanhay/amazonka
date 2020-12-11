{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata related to the given identity, including when the identity was created and any associated linked logins.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DescribeIdentity
  ( -- * Creating a request
    DescribeIdentity (..),
    mkDescribeIdentity,

    -- ** Request lenses
    diIdentityId,

    -- * Destructuring the response
    IdentityDescription (..),
    mkIdentityDescription,

    -- ** Response lenses
    idLastModifiedDate,
    idCreationDate,
    idLogins,
    idIdentityId,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @DescribeIdentity@ action.
--
-- /See:/ 'mkDescribeIdentity' smart constructor.
newtype DescribeIdentity = DescribeIdentity'
  { identityId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeIdentity' with the minimum fields required to make a request.
--
-- * 'identityId' - A unique identifier in the format REGION:GUID.
mkDescribeIdentity ::
  -- | 'identityId'
  Lude.Text ->
  DescribeIdentity
mkDescribeIdentity pIdentityId_ =
  DescribeIdentity' {identityId = pIdentityId_}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIdentityId :: Lens.Lens' DescribeIdentity Lude.Text
diIdentityId = Lens.lens (identityId :: DescribeIdentity -> Lude.Text) (\s a -> s {identityId = a} :: DescribeIdentity)
{-# DEPRECATED diIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

instance Lude.AWSRequest DescribeIdentity where
  type Rs DescribeIdentity = IdentityDescription
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeIdentity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.DescribeIdentity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeIdentity where
  toJSON DescribeIdentity' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("IdentityId" Lude..= identityId)])

instance Lude.ToPath DescribeIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeIdentity where
  toQuery = Lude.const Lude.mempty
