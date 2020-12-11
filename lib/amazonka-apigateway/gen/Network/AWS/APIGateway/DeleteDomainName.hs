{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'DomainName' resource.
module Network.AWS.APIGateway.DeleteDomainName
  ( -- * Creating a request
    DeleteDomainName (..),
    mkDeleteDomainName,

    -- ** Request lenses
    ddnDomainName,

    -- * Destructuring the response
    DeleteDomainNameResponse (..),
    mkDeleteDomainNameResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to delete the 'DomainName' resource.
--
-- /See:/ 'mkDeleteDomainName' smart constructor.
newtype DeleteDomainName = DeleteDomainName'
  { domainName ::
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

-- | Creates a value of 'DeleteDomainName' with the minimum fields required to make a request.
--
-- * 'domainName' - [Required] The name of the 'DomainName' resource to be deleted.
mkDeleteDomainName ::
  -- | 'domainName'
  Lude.Text ->
  DeleteDomainName
mkDeleteDomainName pDomainName_ =
  DeleteDomainName' {domainName = pDomainName_}

-- | [Required] The name of the 'DomainName' resource to be deleted.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddnDomainName :: Lens.Lens' DeleteDomainName Lude.Text
ddnDomainName = Lens.lens (domainName :: DeleteDomainName -> Lude.Text) (\s a -> s {domainName = a} :: DeleteDomainName)
{-# DEPRECATED ddnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteDomainName where
  type Rs DeleteDomainName = DeleteDomainNameResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteDomainNameResponse'

instance Lude.ToHeaders DeleteDomainName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteDomainName where
  toPath DeleteDomainName' {..} =
    Lude.mconcat ["/domainnames/", Lude.toBS domainName]

instance Lude.ToQuery DeleteDomainName where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDomainNameResponse' smart constructor.
data DeleteDomainNameResponse = DeleteDomainNameResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainNameResponse' with the minimum fields required to make a request.
mkDeleteDomainNameResponse ::
  DeleteDomainNameResponse
mkDeleteDomainNameResponse = DeleteDomainNameResponse'
