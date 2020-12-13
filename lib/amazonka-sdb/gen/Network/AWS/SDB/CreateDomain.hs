{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateDomain@ operation creates a new domain. The domain name should be unique among the domains associated with the Access Key ID provided in the request. The @CreateDomain@ operation may take 10 or more seconds to complete.
--
-- The client can create up to 100 domains per account.
-- If the client requires additional domains, go to <http://aws.amazon.com/contact-us/simpledb-limit-request/ http://aws.amazon.com/contact-us/simpledb-limit-request/> .
module Network.AWS.SDB.CreateDomain
  ( -- * Creating a request
    CreateDomain (..),
    mkCreateDomain,

    -- ** Request lenses
    cdDomainName,

    -- * Destructuring the response
    CreateDomainResponse (..),
    mkCreateDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkCreateDomain' smart constructor.
newtype CreateDomain = CreateDomain'
  { -- | The name of the domain to create. The name can range between 3 and 255 characters and can contain the following characters: a-z, A-Z, 0-9, '_', '-', and '.'.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain to create. The name can range between 3 and 255 characters and can contain the following characters: a-z, A-Z, 0-9, '_', '-', and '.'.
mkCreateDomain ::
  -- | 'domainName'
  Lude.Text ->
  CreateDomain
mkCreateDomain pDomainName_ =
  CreateDomain' {domainName = pDomainName_}

-- | The name of the domain to create. The name can range between 3 and 255 characters and can contain the following characters: a-z, A-Z, 0-9, '_', '-', and '.'.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CreateDomain Lude.Text
cdDomainName = Lens.lens (domainName :: CreateDomain -> Lude.Text) (\s a -> s {domainName = a} :: CreateDomain)
{-# DEPRECATED cdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request = Req.postQuery sdbService
  response = Res.receiveNull CreateDomainResponse'

instance Lude.ToHeaders CreateDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDomain where
  toQuery CreateDomain' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
mkCreateDomainResponse ::
  CreateDomainResponse
mkCreateDomainResponse = CreateDomainResponse'
