{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the public key from an RSA key pair that you created with a third-party tool. Compare this with 'CreateKeyPair' , in which AWS creates the key pair and gives the keys to you (AWS keeps a copy of the public key). With ImportKeyPair, you create the key pair and give AWS just the public key. The private key is never transferred between you and AWS.
--
-- For more information about key pairs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ImportKeyPair
  ( -- * Creating a request
    ImportKeyPair (..),
    mkImportKeyPair,

    -- ** Request lenses
    ikpTagSpecifications,
    ikpDryRun,
    ikpKeyName,
    ikpPublicKeyMaterial,

    -- * Destructuring the response
    ImportKeyPairResponse (..),
    mkImportKeyPairResponse,

    -- ** Response lenses
    ikprsKeyFingerprint,
    ikprsKeyName,
    ikprsKeyPairId,
    ikprsTags,
    ikprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    keyName :: Lude.Text,
    publicKeyMaterial :: Lude.Base64
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportKeyPair' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'keyName' - A unique name for the key pair.
-- * 'publicKeyMaterial' - The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'tagSpecifications' - The tags to apply to the imported key pair.
mkImportKeyPair ::
  -- | 'keyName'
  Lude.Text ->
  -- | 'publicKeyMaterial'
  Lude.Base64 ->
  ImportKeyPair
mkImportKeyPair pKeyName_ pPublicKeyMaterial_ =
  ImportKeyPair'
    { tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      keyName = pKeyName_,
      publicKeyMaterial = pPublicKeyMaterial_
    }

-- | The tags to apply to the imported key pair.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpTagSpecifications :: Lens.Lens' ImportKeyPair (Lude.Maybe [TagSpecification])
ikpTagSpecifications = Lens.lens (tagSpecifications :: ImportKeyPair -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: ImportKeyPair)
{-# DEPRECATED ikpTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpDryRun :: Lens.Lens' ImportKeyPair (Lude.Maybe Lude.Bool)
ikpDryRun = Lens.lens (dryRun :: ImportKeyPair -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportKeyPair)
{-# DEPRECATED ikpDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A unique name for the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpKeyName :: Lens.Lens' ImportKeyPair Lude.Text
ikpKeyName = Lens.lens (keyName :: ImportKeyPair -> Lude.Text) (\s a -> s {keyName = a} :: ImportKeyPair)
{-# DEPRECATED ikpKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The public key. For API calls, the text must be base64-encoded. For command line tools, base64 encoding is performed for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'publicKeyMaterial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikpPublicKeyMaterial :: Lens.Lens' ImportKeyPair Lude.Base64
ikpPublicKeyMaterial = Lens.lens (publicKeyMaterial :: ImportKeyPair -> Lude.Base64) (\s a -> s {publicKeyMaterial = a} :: ImportKeyPair)
{-# DEPRECATED ikpPublicKeyMaterial "Use generic-lens or generic-optics with 'publicKeyMaterial' instead." #-}

instance Lude.AWSRequest ImportKeyPair where
  type Rs ImportKeyPair = ImportKeyPairResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ImportKeyPairResponse'
            Lude.<$> (x Lude..@? "keyFingerprint")
            Lude.<*> (x Lude..@? "keyName")
            Lude.<*> (x Lude..@? "keyPairId")
            Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportKeyPair where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportKeyPair where
  toQuery ImportKeyPair' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ImportKeyPair" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "KeyName" Lude.=: keyName,
        "PublicKeyMaterial" Lude.=: publicKeyMaterial
      ]

-- | /See:/ 'mkImportKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { keyFingerprint ::
      Lude.Maybe Lude.Text,
    keyName :: Lude.Maybe Lude.Text,
    keyPairId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'keyFingerprint' - The MD5 public key fingerprint as specified in section 4 of RFC 4716.
-- * 'keyName' - The key pair name you provided.
-- * 'keyPairId' - The ID of the resulting key pair.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags applied to the imported key pair.
mkImportKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportKeyPairResponse
mkImportKeyPairResponse pResponseStatus_ =
  ImportKeyPairResponse'
    { keyFingerprint = Lude.Nothing,
      keyName = Lude.Nothing,
      keyPairId = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The MD5 public key fingerprint as specified in section 4 of RFC 4716.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsKeyFingerprint :: Lens.Lens' ImportKeyPairResponse (Lude.Maybe Lude.Text)
ikprsKeyFingerprint = Lens.lens (keyFingerprint :: ImportKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyFingerprint = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsKeyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead." #-}

-- | The key pair name you provided.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsKeyName :: Lens.Lens' ImportKeyPairResponse (Lude.Maybe Lude.Text)
ikprsKeyName = Lens.lens (keyName :: ImportKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The ID of the resulting key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsKeyPairId :: Lens.Lens' ImportKeyPairResponse (Lude.Maybe Lude.Text)
ikprsKeyPairId = Lens.lens (keyPairId :: ImportKeyPairResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyPairId = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsKeyPairId "Use generic-lens or generic-optics with 'keyPairId' instead." #-}

-- | The tags applied to the imported key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsTags :: Lens.Lens' ImportKeyPairResponse (Lude.Maybe [Tag])
ikprsTags = Lens.lens (tags :: ImportKeyPairResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ikprsResponseStatus :: Lens.Lens' ImportKeyPairResponse Lude.Int
ikprsResponseStatus = Lens.lens (responseStatus :: ImportKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportKeyPairResponse)
{-# DEPRECATED ikprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
