{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/creating-domains.html Creating a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.CreateDomain
  ( -- * Creating a request
    CreateDomain (..),
    mkCreateDomain,

    -- ** Request lenses
    cdDomainName,

    -- * Destructuring the response
    CreateDomainResponse (..),
    mkCreateDomainResponse,

    -- ** Response lenses
    cdrsDomainStatus,
    cdrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'CreateDomain' @ operation. Specifies a name for the new search domain.
--
-- /See:/ 'mkCreateDomain' smart constructor.
newtype CreateDomain = CreateDomain'
  { -- | A name for the domain you are creating. Allowed characters are a-z (lower-case letters), 0-9, and hyphen (-). Domain names must start with a letter or number and be at least 3 and no more than 28 characters long.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - A name for the domain you are creating. Allowed characters are a-z (lower-case letters), 0-9, and hyphen (-). Domain names must start with a letter or number and be at least 3 and no more than 28 characters long.
mkCreateDomain ::
  -- | 'domainName'
  Lude.Text ->
  CreateDomain
mkCreateDomain pDomainName_ =
  CreateDomain' {domainName = pDomainName_}

-- | A name for the domain you are creating. Allowed characters are a-z (lower-case letters), 0-9, and hyphen (-). Domain names must start with a letter or number and be at least 3 and no more than 28 characters long.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CreateDomain Lude.Text
cdDomainName = Lens.lens (domainName :: CreateDomain -> Lude.Text) (\s a -> s {domainName = a} :: CreateDomain)
{-# DEPRECATED cdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "CreateDomainResult"
      ( \s h x ->
          CreateDomainResponse'
            Lude.<$> (x Lude..@? "DomainStatus") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDomain where
  toQuery CreateDomain' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @CreateDomainRequest@ . Contains the status of a newly created domain.
--
-- /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { domainStatus :: Lude.Maybe DomainStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainStatus' -
-- * 'responseStatus' - The response status code.
mkCreateDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDomainResponse
mkCreateDomainResponse pResponseStatus_ =
  CreateDomainResponse'
    { domainStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDomainStatus :: Lens.Lens' CreateDomainResponse (Lude.Maybe DomainStatus)
cdrsDomainStatus = Lens.lens (domainStatus :: CreateDomainResponse -> Lude.Maybe DomainStatus) (\s a -> s {domainStatus = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDomainResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
