{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteDomain@ operation deletes a domain. Any items (and their attributes) in the domain are deleted as well. The @DeleteDomain@ operation might take 10 or more seconds to complete.
module Network.AWS.SDB.DeleteDomain
  ( -- * Creating a request
    DeleteDomain (..),
    mkDeleteDomain,

    -- ** Request lenses
    ddDomainName,

    -- * Destructuring the response
    DeleteDomainResponse (..),
    mkDeleteDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain' {domainName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain to delete.
mkDeleteDomain ::
  -- | 'domainName'
  Lude.Text ->
  DeleteDomain
mkDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | The name of the domain to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Lude.Text
ddDomainName = Lens.lens (domainName :: DeleteDomain -> Lude.Text) (\s a -> s {domainName = a} :: DeleteDomain)
{-# DEPRECATED ddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = Req.postQuery sdbService
  response = Res.receiveNull DeleteDomainResponse'

instance Lude.ToHeaders DeleteDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDomain where
  toQuery DeleteDomain' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
mkDeleteDomainResponse ::
  DeleteDomainResponse
mkDeleteDomainResponse = DeleteDomainResponse'
