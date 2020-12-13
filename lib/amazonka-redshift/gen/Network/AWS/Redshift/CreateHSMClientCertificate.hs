{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateHSMClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM client certificate that an Amazon Redshift cluster will use to connect to the client's HSM in order to store and retrieve the keys used to encrypt the cluster databases.
--
-- The command returns a public key, which you must store in the HSM. In addition to creating the HSM certificate, you must create an Amazon Redshift HSM configuration that provides a cluster the information needed to store and use encryption keys in the HSM. For more information, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
module Network.AWS.Redshift.CreateHSMClientCertificate
  ( -- * Creating a request
    CreateHSMClientCertificate (..),
    mkCreateHSMClientCertificate,

    -- ** Request lenses
    chccHSMClientCertificateIdentifier,
    chccTags,

    -- * Destructuring the response
    CreateHSMClientCertificateResponse (..),
    mkCreateHSMClientCertificateResponse,

    -- ** Response lenses
    chccrsHSMClientCertificate,
    chccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateHSMClientCertificate' smart constructor.
data CreateHSMClientCertificate = CreateHSMClientCertificate'
  { -- | The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
    hsmClientCertificateIdentifier :: Lude.Text,
    -- | A list of tag instances.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHSMClientCertificate' with the minimum fields required to make a request.
--
-- * 'hsmClientCertificateIdentifier' - The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
-- * 'tags' - A list of tag instances.
mkCreateHSMClientCertificate ::
  -- | 'hsmClientCertificateIdentifier'
  Lude.Text ->
  CreateHSMClientCertificate
mkCreateHSMClientCertificate pHSMClientCertificateIdentifier_ =
  CreateHSMClientCertificate'
    { hsmClientCertificateIdentifier =
        pHSMClientCertificateIdentifier_,
      tags = Lude.Nothing
    }

-- | The identifier to be assigned to the new HSM client certificate that the cluster will use to connect to the HSM to use the database encryption keys.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccHSMClientCertificateIdentifier :: Lens.Lens' CreateHSMClientCertificate Lude.Text
chccHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: CreateHSMClientCertificate -> Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: CreateHSMClientCertificate)
{-# DEPRECATED chccHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccTags :: Lens.Lens' CreateHSMClientCertificate (Lude.Maybe [Tag])
chccTags = Lens.lens (tags :: CreateHSMClientCertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateHSMClientCertificate)
{-# DEPRECATED chccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateHSMClientCertificate where
  type
    Rs CreateHSMClientCertificate =
      CreateHSMClientCertificateResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateHsmClientCertificateResult"
      ( \s h x ->
          CreateHSMClientCertificateResponse'
            Lude.<$> (x Lude..@? "HsmClientCertificate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHSMClientCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateHSMClientCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHSMClientCertificate where
  toQuery CreateHSMClientCertificate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateHsmClientCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "HsmClientCertificateIdentifier"
          Lude.=: hsmClientCertificateIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateHSMClientCertificateResponse' smart constructor.
data CreateHSMClientCertificateResponse = CreateHSMClientCertificateResponse'
  { hsmClientCertificate :: Lude.Maybe HSMClientCertificate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHSMClientCertificateResponse' with the minimum fields required to make a request.
--
-- * 'hsmClientCertificate' -
-- * 'responseStatus' - The response status code.
mkCreateHSMClientCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHSMClientCertificateResponse
mkCreateHSMClientCertificateResponse pResponseStatus_ =
  CreateHSMClientCertificateResponse'
    { hsmClientCertificate =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'hsmClientCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccrsHSMClientCertificate :: Lens.Lens' CreateHSMClientCertificateResponse (Lude.Maybe HSMClientCertificate)
chccrsHSMClientCertificate = Lens.lens (hsmClientCertificate :: CreateHSMClientCertificateResponse -> Lude.Maybe HSMClientCertificate) (\s a -> s {hsmClientCertificate = a} :: CreateHSMClientCertificateResponse)
{-# DEPRECATED chccrsHSMClientCertificate "Use generic-lens or generic-optics with 'hsmClientCertificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chccrsResponseStatus :: Lens.Lens' CreateHSMClientCertificateResponse Lude.Int
chccrsResponseStatus = Lens.lens (responseStatus :: CreateHSMClientCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHSMClientCertificateResponse)
{-# DEPRECATED chccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
