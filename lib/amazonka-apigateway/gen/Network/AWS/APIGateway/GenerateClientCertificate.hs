{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GenerateClientCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a 'ClientCertificate' resource.
module Network.AWS.APIGateway.GenerateClientCertificate
  ( -- * Creating a request
    GenerateClientCertificate (..),
    mkGenerateClientCertificate,

    -- ** Request lenses
    gccDescription,
    gccTags,

    -- * Destructuring the response
    ClientCertificate (..),
    mkClientCertificate,

    -- ** Response lenses
    ccPemEncodedCertificate,
    ccClientCertificateId,
    ccCreatedDate,
    ccExpirationDate,
    ccDescription,
    ccTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to generate a 'ClientCertificate' resource.
--
-- /See:/ 'mkGenerateClientCertificate' smart constructor.
data GenerateClientCertificate = GenerateClientCertificate'
  { -- | The description of the 'ClientCertificate' .
    description :: Lude.Maybe Lude.Text,
    -- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateClientCertificate' with the minimum fields required to make a request.
--
-- * 'description' - The description of the 'ClientCertificate' .
-- * 'tags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
mkGenerateClientCertificate ::
  GenerateClientCertificate
mkGenerateClientCertificate =
  GenerateClientCertificate'
    { description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The description of the 'ClientCertificate' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDescription :: Lens.Lens' GenerateClientCertificate (Lude.Maybe Lude.Text)
gccDescription = Lens.lens (description :: GenerateClientCertificate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GenerateClientCertificate)
{-# DEPRECATED gccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccTags :: Lens.Lens' GenerateClientCertificate (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gccTags = Lens.lens (tags :: GenerateClientCertificate -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GenerateClientCertificate)
{-# DEPRECATED gccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest GenerateClientCertificate where
  type Rs GenerateClientCertificate = ClientCertificate
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GenerateClientCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON GenerateClientCertificate where
  toJSON GenerateClientCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath GenerateClientCertificate where
  toPath = Lude.const "/clientcertificates"

instance Lude.ToQuery GenerateClientCertificate where
  toQuery = Lude.const Lude.mempty
