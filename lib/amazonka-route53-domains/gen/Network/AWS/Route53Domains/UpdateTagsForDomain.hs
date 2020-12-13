{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateTagsForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds or updates tags for a specified domain.
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
module Network.AWS.Route53Domains.UpdateTagsForDomain
  ( -- * Creating a request
    UpdateTagsForDomain (..),
    mkUpdateTagsForDomain,

    -- ** Request lenses
    utfdTagsToUpdate,
    utfdDomainName,

    -- * Destructuring the response
    UpdateTagsForDomainResponse (..),
    mkUpdateTagsForDomainResponse,

    -- ** Response lenses
    utfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The UpdateTagsForDomainRequest includes the following elements.
--
-- /See:/ 'mkUpdateTagsForDomain' smart constructor.
data UpdateTagsForDomain = UpdateTagsForDomain'
  { -- | A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
    tagsToUpdate :: Lude.Maybe [Tag],
    -- | The domain for which you want to add or update tags.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTagsForDomain' with the minimum fields required to make a request.
--
-- * 'tagsToUpdate' - A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
-- * 'domainName' - The domain for which you want to add or update tags.
mkUpdateTagsForDomain ::
  -- | 'domainName'
  Lude.Text ->
  UpdateTagsForDomain
mkUpdateTagsForDomain pDomainName_ =
  UpdateTagsForDomain'
    { tagsToUpdate = Lude.Nothing,
      domainName = pDomainName_
    }

-- | A list of the tag keys and values that you want to add or update. If you specify a key that already exists, the corresponding value will be replaced.
--
-- /Note:/ Consider using 'tagsToUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfdTagsToUpdate :: Lens.Lens' UpdateTagsForDomain (Lude.Maybe [Tag])
utfdTagsToUpdate = Lens.lens (tagsToUpdate :: UpdateTagsForDomain -> Lude.Maybe [Tag]) (\s a -> s {tagsToUpdate = a} :: UpdateTagsForDomain)
{-# DEPRECATED utfdTagsToUpdate "Use generic-lens or generic-optics with 'tagsToUpdate' instead." #-}

-- | The domain for which you want to add or update tags.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfdDomainName :: Lens.Lens' UpdateTagsForDomain Lude.Text
utfdDomainName = Lens.lens (domainName :: UpdateTagsForDomain -> Lude.Text) (\s a -> s {domainName = a} :: UpdateTagsForDomain)
{-# DEPRECATED utfdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest UpdateTagsForDomain where
  type Rs UpdateTagsForDomain = UpdateTagsForDomainResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateTagsForDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTagsForDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.UpdateTagsForDomain" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTagsForDomain where
  toJSON UpdateTagsForDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TagsToUpdate" Lude..=) Lude.<$> tagsToUpdate,
            Lude.Just ("DomainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath UpdateTagsForDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTagsForDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTagsForDomainResponse' smart constructor.
newtype UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTagsForDomainResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateTagsForDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTagsForDomainResponse
mkUpdateTagsForDomainResponse pResponseStatus_ =
  UpdateTagsForDomainResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utfdrsResponseStatus :: Lens.Lens' UpdateTagsForDomainResponse Lude.Int
utfdrsResponseStatus = Lens.lens (responseStatus :: UpdateTagsForDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTagsForDomainResponse)
{-# DEPRECATED utfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
