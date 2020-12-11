{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListGroupCertificateAuthorities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current CAs for a group.
module Network.AWS.Greengrass.ListGroupCertificateAuthorities
  ( -- * Creating a request
    ListGroupCertificateAuthorities (..),
    mkListGroupCertificateAuthorities,

    -- ** Request lenses
    lgcaGroupId,

    -- * Destructuring the response
    ListGroupCertificateAuthoritiesResponse (..),
    mkListGroupCertificateAuthoritiesResponse,

    -- ** Response lenses
    lgcarsGroupCertificateAuthorities,
    lgcarsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroupCertificateAuthorities' smart constructor.
newtype ListGroupCertificateAuthorities = ListGroupCertificateAuthorities'
  { groupId ::
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

-- | Creates a value of 'ListGroupCertificateAuthorities' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
mkListGroupCertificateAuthorities ::
  -- | 'groupId'
  Lude.Text ->
  ListGroupCertificateAuthorities
mkListGroupCertificateAuthorities pGroupId_ =
  ListGroupCertificateAuthorities' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgcaGroupId :: Lens.Lens' ListGroupCertificateAuthorities Lude.Text
lgcaGroupId = Lens.lens (groupId :: ListGroupCertificateAuthorities -> Lude.Text) (\s a -> s {groupId = a} :: ListGroupCertificateAuthorities)
{-# DEPRECATED lgcaGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest ListGroupCertificateAuthorities where
  type
    Rs ListGroupCertificateAuthorities =
      ListGroupCertificateAuthoritiesResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGroupCertificateAuthoritiesResponse'
            Lude.<$> (x Lude..?> "GroupCertificateAuthorities" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGroupCertificateAuthorities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListGroupCertificateAuthorities where
  toPath ListGroupCertificateAuthorities' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/certificateauthorities"
      ]

instance Lude.ToQuery ListGroupCertificateAuthorities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGroupCertificateAuthoritiesResponse' smart constructor.
data ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse'
  { groupCertificateAuthorities ::
      Lude.Maybe
        [GroupCertificateAuthorityProperties],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupCertificateAuthoritiesResponse' with the minimum fields required to make a request.
--
-- * 'groupCertificateAuthorities' - A list of certificate authorities associated with the group.
-- * 'responseStatus' - The response status code.
mkListGroupCertificateAuthoritiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupCertificateAuthoritiesResponse
mkListGroupCertificateAuthoritiesResponse pResponseStatus_ =
  ListGroupCertificateAuthoritiesResponse'
    { groupCertificateAuthorities =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of certificate authorities associated with the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgcarsGroupCertificateAuthorities :: Lens.Lens' ListGroupCertificateAuthoritiesResponse (Lude.Maybe [GroupCertificateAuthorityProperties])
lgcarsGroupCertificateAuthorities = Lens.lens (groupCertificateAuthorities :: ListGroupCertificateAuthoritiesResponse -> Lude.Maybe [GroupCertificateAuthorityProperties]) (\s a -> s {groupCertificateAuthorities = a} :: ListGroupCertificateAuthoritiesResponse)
{-# DEPRECATED lgcarsGroupCertificateAuthorities "Use generic-lens or generic-optics with 'groupCertificateAuthorities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgcarsResponseStatus :: Lens.Lens' ListGroupCertificateAuthoritiesResponse Lude.Int
lgcarsResponseStatus = Lens.lens (responseStatus :: ListGroupCertificateAuthoritiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupCertificateAuthoritiesResponse)
{-# DEPRECATED lgcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
