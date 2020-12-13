{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListRetirableGrants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all grants for which the grant's @RetiringPrincipal@ matches the one specified.
--
-- A typical use is to list all grants that you are able to retire. To retire a grant, use 'RetireGrant' .
module Network.AWS.KMS.ListRetirableGrants
  ( -- * Creating a request
    ListRetirableGrants (..),
    mkListRetirableGrants,

    -- ** Request lenses
    lrgRetiringPrincipal,
    lrgMarker,
    lrgLimit,

    -- * Destructuring the response
    ListGrantsResponse (..),
    mkListGrantsResponse,

    -- ** Response lenses
    lgTruncated,
    lgGrants,
    lgNextMarker,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRetirableGrants' smart constructor.
data ListRetirableGrants = ListRetirableGrants'
  { -- | The retiring principal for which to list grants.
    --
    -- To specify the retiring principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
    retiringPrincipal :: Lude.Text,
    -- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
    --
    -- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRetirableGrants' with the minimum fields required to make a request.
--
-- * 'retiringPrincipal' - The retiring principal for which to list grants.
--
-- To specify the retiring principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
-- * 'limit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
mkListRetirableGrants ::
  -- | 'retiringPrincipal'
  Lude.Text ->
  ListRetirableGrants
mkListRetirableGrants pRetiringPrincipal_ =
  ListRetirableGrants'
    { retiringPrincipal = pRetiringPrincipal_,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The retiring principal for which to list grants.
--
-- To specify the retiring principal, use the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'retiringPrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgRetiringPrincipal :: Lens.Lens' ListRetirableGrants Lude.Text
lrgRetiringPrincipal = Lens.lens (retiringPrincipal :: ListRetirableGrants -> Lude.Text) (\s a -> s {retiringPrincipal = a} :: ListRetirableGrants)
{-# DEPRECATED lrgRetiringPrincipal "Use generic-lens or generic-optics with 'retiringPrincipal' instead." #-}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgMarker :: Lens.Lens' ListRetirableGrants (Lude.Maybe Lude.Text)
lrgMarker = Lens.lens (marker :: ListRetirableGrants -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRetirableGrants)
{-# DEPRECATED lrgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgLimit :: Lens.Lens' ListRetirableGrants (Lude.Maybe Lude.Natural)
lrgLimit = Lens.lens (limit :: ListRetirableGrants -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListRetirableGrants)
{-# DEPRECATED lrgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest ListRetirableGrants where
  type Rs ListRetirableGrants = ListGrantsResponse
  request = Req.postJSON kmsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders ListRetirableGrants where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ListRetirableGrants" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRetirableGrants where
  toJSON ListRetirableGrants' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RetiringPrincipal" Lude..= retiringPrincipal),
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListRetirableGrants where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRetirableGrants where
  toQuery = Lude.const Lude.mempty
