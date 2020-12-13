{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SAML provider resource in IAM.
--
-- Deleting the provider resource from IAM does not update any roles that reference the SAML provider resource's ARN as a principal in their trust policies. Any attempt to assume a role that references a non-existent provider resource ARN fails.
module Network.AWS.IAM.DeleteSAMLProvider
  ( -- * Creating a request
    DeleteSAMLProvider (..),
    mkDeleteSAMLProvider,

    -- ** Request lenses
    dsamlpSAMLProviderARN,

    -- * Destructuring the response
    DeleteSAMLProviderResponse (..),
    mkDeleteSAMLProviderResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSAMLProvider' smart constructor.
newtype DeleteSAMLProvider = DeleteSAMLProvider'
  { -- | The Amazon Resource Name (ARN) of the SAML provider to delete.
    sAMLProviderARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSAMLProvider' with the minimum fields required to make a request.
--
-- * 'sAMLProviderARN' - The Amazon Resource Name (ARN) of the SAML provider to delete.
mkDeleteSAMLProvider ::
  -- | 'sAMLProviderARN'
  Lude.Text ->
  DeleteSAMLProvider
mkDeleteSAMLProvider pSAMLProviderARN_ =
  DeleteSAMLProvider' {sAMLProviderARN = pSAMLProviderARN_}

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
--
-- /Note:/ Consider using 'sAMLProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsamlpSAMLProviderARN :: Lens.Lens' DeleteSAMLProvider Lude.Text
dsamlpSAMLProviderARN = Lens.lens (sAMLProviderARN :: DeleteSAMLProvider -> Lude.Text) (\s a -> s {sAMLProviderARN = a} :: DeleteSAMLProvider)
{-# DEPRECATED dsamlpSAMLProviderARN "Use generic-lens or generic-optics with 'sAMLProviderARN' instead." #-}

instance Lude.AWSRequest DeleteSAMLProvider where
  type Rs DeleteSAMLProvider = DeleteSAMLProviderResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteSAMLProviderResponse'

instance Lude.ToHeaders DeleteSAMLProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSAMLProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSAMLProvider where
  toQuery DeleteSAMLProvider' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSAMLProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "SAMLProviderArn" Lude.=: sAMLProviderARN
      ]

-- | /See:/ 'mkDeleteSAMLProviderResponse' smart constructor.
data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSAMLProviderResponse' with the minimum fields required to make a request.
mkDeleteSAMLProviderResponse ::
  DeleteSAMLProviderResponse
mkDeleteSAMLProviderResponse = DeleteSAMLProviderResponse'
