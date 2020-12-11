{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified directory, lists all the certificates registered for a secured LDAP connection.
module Network.AWS.DirectoryService.ListCertificates
  ( -- * Creating a request
    ListCertificates (..),
    mkListCertificates,

    -- ** Request lenses
    lcNextToken,
    lcLimit,
    lcDirectoryId,

    -- * Destructuring the response
    ListCertificatesResponse (..),
    mkListCertificatesResponse,

    -- ** Response lenses
    lcrsNextToken,
    lcrsCertificatesInfo,
    lcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    directoryId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'limit' - The number of items that should show up on one page
-- * 'nextToken' - A token for requesting another page of certificates if the @NextToken@ response element indicates that more certificates are available. Use the value of the returned @NextToken@ element in your request until the token comes back as @null@ . Pass @null@ if this is the first call.
mkListCertificates ::
  -- | 'directoryId'
  Lude.Text ->
  ListCertificates
mkListCertificates pDirectoryId_ =
  ListCertificates'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      directoryId = pDirectoryId_
    }

-- | A token for requesting another page of certificates if the @NextToken@ response element indicates that more certificates are available. Use the value of the returned @NextToken@ element in your request until the token comes back as @null@ . Pass @null@ if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListCertificates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCertificates)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of items that should show up on one page
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLimit :: Lens.Lens' ListCertificates (Lude.Maybe Lude.Natural)
lcLimit = Lens.lens (limit :: ListCertificates -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListCertificates)
{-# DEPRECATED lcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcDirectoryId :: Lens.Lens' ListCertificates Lude.Text
lcDirectoryId = Lens.lens (directoryId :: ListCertificates -> Lude.Text) (\s a -> s {directoryId = a} :: ListCertificates)
{-# DEPRECATED lcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CertificatesInfo" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCertificates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.ListCertificates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath ListCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCertificates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    certificatesInfo ::
      Lude.Maybe [CertificateInfo],
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

-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'certificatesInfo' - A list of certificates with basic details including certificate ID, certificate common name, certificate state.
-- * 'nextToken' - Indicates whether another page of certificates is available when the number of available certificates exceeds the page limit.
-- * 'responseStatus' - The response status code.
mkListCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCertificatesResponse
mkListCertificatesResponse pResponseStatus_ =
  ListCertificatesResponse'
    { nextToken = Lude.Nothing,
      certificatesInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether another page of certificates is available when the number of available certificates exceeds the page limit.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListCertificatesResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of certificates with basic details including certificate ID, certificate common name, certificate state.
--
-- /Note:/ Consider using 'certificatesInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsCertificatesInfo :: Lens.Lens' ListCertificatesResponse (Lude.Maybe [CertificateInfo])
lcrsCertificatesInfo = Lens.lens (certificatesInfo :: ListCertificatesResponse -> Lude.Maybe [CertificateInfo]) (\s a -> s {certificatesInfo = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsCertificatesInfo "Use generic-lens or generic-optics with 'certificatesInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListCertificatesResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCertificatesResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
