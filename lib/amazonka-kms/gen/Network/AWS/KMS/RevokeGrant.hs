{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.RevokeGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the specified grant for the specified customer master key (CMK). You can revoke a grant to actively deny operations that depend on it.
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter.
module Network.AWS.KMS.RevokeGrant
  ( -- * Creating a request
    RevokeGrant (..),
    mkRevokeGrant,

    -- ** Request lenses
    rKeyId,
    rGrantId,

    -- * Destructuring the response
    RevokeGrantResponse (..),
    mkRevokeGrantResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRevokeGrant' smart constructor.
data RevokeGrant = RevokeGrant'
  { keyId :: Lude.Text,
    grantId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeGrant' with the minimum fields required to make a request.
--
-- * 'grantId' - Identifier of the grant to be revoked.
-- * 'keyId' - A unique identifier for the customer master key associated with the grant.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
mkRevokeGrant ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'grantId'
  Lude.Text ->
  RevokeGrant
mkRevokeGrant pKeyId_ pGrantId_ =
  RevokeGrant' {keyId = pKeyId_, grantId = pGrantId_}

-- | A unique identifier for the customer master key associated with the grant.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKeyId :: Lens.Lens' RevokeGrant Lude.Text
rKeyId = Lens.lens (keyId :: RevokeGrant -> Lude.Text) (\s a -> s {keyId = a} :: RevokeGrant)
{-# DEPRECATED rKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Identifier of the grant to be revoked.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGrantId :: Lens.Lens' RevokeGrant Lude.Text
rGrantId = Lens.lens (grantId :: RevokeGrant -> Lude.Text) (\s a -> s {grantId = a} :: RevokeGrant)
{-# DEPRECATED rGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

instance Lude.AWSRequest RevokeGrant where
  type Rs RevokeGrant = RevokeGrantResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull RevokeGrantResponse'

instance Lude.ToHeaders RevokeGrant where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.RevokeGrant" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RevokeGrant where
  toJSON RevokeGrant' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("GrantId" Lude..= grantId)
          ]
      )

instance Lude.ToPath RevokeGrant where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeGrant where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRevokeGrantResponse' smart constructor.
data RevokeGrantResponse = RevokeGrantResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeGrantResponse' with the minimum fields required to make a request.
mkRevokeGrantResponse ::
  RevokeGrantResponse
mkRevokeGrantResponse = RevokeGrantResponse'
