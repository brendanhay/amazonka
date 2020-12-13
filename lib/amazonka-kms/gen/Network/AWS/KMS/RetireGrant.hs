{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.RetireGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retires a grant. To clean up, you can retire a grant when you're done using it. You should revoke a grant when you intend to actively deny operations that depend on it. The following are permitted to call this API:
--
--
--     * The AWS account (root user) under which the grant was created
--
--
--     * The @RetiringPrincipal@ , if present in the grant
--
--
--     * The @GranteePrincipal@ , if @RetireGrant@ is an operation specified in the grant
--
--
-- You must identify the grant to retire by its grant token or by a combination of the grant ID and the Amazon Resource Name (ARN) of the customer master key (CMK). A grant token is a unique variable-length base64-encoded string. A grant ID is a 64 character unique identifier of a grant. The 'CreateGrant' operation returns both.
module Network.AWS.KMS.RetireGrant
  ( -- * Creating a request
    RetireGrant (..),
    mkRetireGrant,

    -- ** Request lenses
    rgKeyId,
    rgGrantId,
    rgGrantToken,

    -- * Destructuring the response
    RetireGrantResponse (..),
    mkRetireGrantResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRetireGrant' smart constructor.
data RetireGrant = RetireGrant'
  { -- | The Amazon Resource Name (ARN) of the CMK associated with the grant.
    --
    -- For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    keyId :: Lude.Maybe Lude.Text,
    -- | Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.
    --
    --
    --     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
    grantId :: Lude.Maybe Lude.Text,
    -- | Token that identifies the grant to be retired.
    grantToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetireGrant' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (ARN) of the CMK associated with the grant.
--
-- For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
-- * 'grantId' - Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.
--
--
--     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
--
--
-- * 'grantToken' - Token that identifies the grant to be retired.
mkRetireGrant ::
  RetireGrant
mkRetireGrant =
  RetireGrant'
    { keyId = Lude.Nothing,
      grantId = Lude.Nothing,
      grantToken = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CMK associated with the grant.
--
-- For example: @arn:aws:kms:us-east-2:444455556666:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgKeyId :: Lens.Lens' RetireGrant (Lude.Maybe Lude.Text)
rgKeyId = Lens.lens (keyId :: RetireGrant -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: RetireGrant)
{-# DEPRECATED rgKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Unique identifier of the grant to retire. The grant ID is returned in the response to a @CreateGrant@ operation.
--
--
--     * Grant ID Example - 0123456789012345678901234567890123456789012345678901234567890123
--
--
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGrantId :: Lens.Lens' RetireGrant (Lude.Maybe Lude.Text)
rgGrantId = Lens.lens (grantId :: RetireGrant -> Lude.Maybe Lude.Text) (\s a -> s {grantId = a} :: RetireGrant)
{-# DEPRECATED rgGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

-- | Token that identifies the grant to be retired.
--
-- /Note:/ Consider using 'grantToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgGrantToken :: Lens.Lens' RetireGrant (Lude.Maybe Lude.Text)
rgGrantToken = Lens.lens (grantToken :: RetireGrant -> Lude.Maybe Lude.Text) (\s a -> s {grantToken = a} :: RetireGrant)
{-# DEPRECATED rgGrantToken "Use generic-lens or generic-optics with 'grantToken' instead." #-}

instance Lude.AWSRequest RetireGrant where
  type Rs RetireGrant = RetireGrantResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull RetireGrantResponse'

instance Lude.ToHeaders RetireGrant where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.RetireGrant" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RetireGrant where
  toJSON RetireGrant' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyId" Lude..=) Lude.<$> keyId,
            ("GrantId" Lude..=) Lude.<$> grantId,
            ("GrantToken" Lude..=) Lude.<$> grantToken
          ]
      )

instance Lude.ToPath RetireGrant where
  toPath = Lude.const "/"

instance Lude.ToQuery RetireGrant where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRetireGrantResponse' smart constructor.
data RetireGrantResponse = RetireGrantResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetireGrantResponse' with the minimum fields required to make a request.
mkRetireGrantResponse ::
  RetireGrantResponse
mkRetireGrantResponse = RetireGrantResponse'
