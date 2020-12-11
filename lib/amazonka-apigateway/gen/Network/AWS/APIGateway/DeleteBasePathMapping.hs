{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'BasePathMapping' resource.
module Network.AWS.APIGateway.DeleteBasePathMapping
  ( -- * Creating a request
    DeleteBasePathMapping (..),
    mkDeleteBasePathMapping,

    -- ** Request lenses
    dbpmDomainName,
    dbpmBasePath,

    -- * Destructuring the response
    DeleteBasePathMappingResponse (..),
    mkDeleteBasePathMappingResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete the 'BasePathMapping' resource.
--
-- /See:/ 'mkDeleteBasePathMapping' smart constructor.
data DeleteBasePathMapping = DeleteBasePathMapping'
  { domainName ::
      Lude.Text,
    basePath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBasePathMapping' with the minimum fields required to make a request.
--
-- * 'basePath' - [Required] The base path name of the 'BasePathMapping' resource to delete.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
-- * 'domainName' - [Required] The domain name of the 'BasePathMapping' resource to delete.
mkDeleteBasePathMapping ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'basePath'
  Lude.Text ->
  DeleteBasePathMapping
mkDeleteBasePathMapping pDomainName_ pBasePath_ =
  DeleteBasePathMapping'
    { domainName = pDomainName_,
      basePath = pBasePath_
    }

-- | [Required] The domain name of the 'BasePathMapping' resource to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpmDomainName :: Lens.Lens' DeleteBasePathMapping Lude.Text
dbpmDomainName = Lens.lens (domainName :: DeleteBasePathMapping -> Lude.Text) (\s a -> s {domainName = a} :: DeleteBasePathMapping)
{-# DEPRECATED dbpmDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | [Required] The base path name of the 'BasePathMapping' resource to delete.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpmBasePath :: Lens.Lens' DeleteBasePathMapping Lude.Text
dbpmBasePath = Lens.lens (basePath :: DeleteBasePathMapping -> Lude.Text) (\s a -> s {basePath = a} :: DeleteBasePathMapping)
{-# DEPRECATED dbpmBasePath "Use generic-lens or generic-optics with 'basePath' instead." #-}

instance Lude.AWSRequest DeleteBasePathMapping where
  type Rs DeleteBasePathMapping = DeleteBasePathMappingResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteBasePathMappingResponse'

instance Lude.ToHeaders DeleteBasePathMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteBasePathMapping where
  toPath DeleteBasePathMapping' {..} =
    Lude.mconcat
      [ "/domainnames/",
        Lude.toBS domainName,
        "/basepathmappings/",
        Lude.toBS basePath
      ]

instance Lude.ToQuery DeleteBasePathMapping where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBasePathMappingResponse' smart constructor.
data DeleteBasePathMappingResponse = DeleteBasePathMappingResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBasePathMappingResponse' with the minimum fields required to make a request.
mkDeleteBasePathMappingResponse ::
  DeleteBasePathMappingResponse
mkDeleteBasePathMappingResponse = DeleteBasePathMappingResponse'
