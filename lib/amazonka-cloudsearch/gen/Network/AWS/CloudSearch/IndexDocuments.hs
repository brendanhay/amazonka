{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.IndexDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tells the search domain to start indexing its documents using the latest indexing options. This operation must be invoked to activate options whose 'OptionStatus' is @RequiresIndexDocuments@ .
module Network.AWS.CloudSearch.IndexDocuments
  ( -- * Creating a request
    IndexDocuments (..),
    mkIndexDocuments,

    -- ** Request lenses
    idDomainName,

    -- * Destructuring the response
    IndexDocumentsResponse (..),
    mkIndexDocumentsResponse,

    -- ** Response lenses
    idrsFieldNames,
    idrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'IndexDocuments' @ operation. Specifies the name of the domain you want to re-index.
--
-- /See:/ 'mkIndexDocuments' smart constructor.
newtype IndexDocuments = IndexDocuments'
  { domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IndexDocuments' with the minimum fields required to make a request.
--
-- * 'domainName' -
mkIndexDocuments ::
  -- | 'domainName'
  Lude.Text ->
  IndexDocuments
mkIndexDocuments pDomainName_ =
  IndexDocuments' {domainName = pDomainName_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idDomainName :: Lens.Lens' IndexDocuments Lude.Text
idDomainName = Lens.lens (domainName :: IndexDocuments -> Lude.Text) (\s a -> s {domainName = a} :: IndexDocuments)
{-# DEPRECATED idDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest IndexDocuments where
  type Rs IndexDocuments = IndexDocumentsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "IndexDocumentsResult"
      ( \s h x ->
          IndexDocumentsResponse'
            Lude.<$> ( x Lude..@? "FieldNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders IndexDocuments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath IndexDocuments where
  toPath = Lude.const "/"

instance Lude.ToQuery IndexDocuments where
  toQuery IndexDocuments' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("IndexDocuments" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | The result of an @IndexDocuments@ request. Contains the status of the indexing operation, including the fields being indexed.
--
-- /See:/ 'mkIndexDocumentsResponse' smart constructor.
data IndexDocumentsResponse = IndexDocumentsResponse'
  { -- | The names of the fields that are currently being indexed.
    fieldNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IndexDocumentsResponse' with the minimum fields required to make a request.
--
-- * 'fieldNames' - The names of the fields that are currently being indexed.
-- * 'responseStatus' - The response status code.
mkIndexDocumentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  IndexDocumentsResponse
mkIndexDocumentsResponse pResponseStatus_ =
  IndexDocumentsResponse'
    { fieldNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the fields that are currently being indexed.
--
-- /Note:/ Consider using 'fieldNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrsFieldNames :: Lens.Lens' IndexDocumentsResponse (Lude.Maybe [Lude.Text])
idrsFieldNames = Lens.lens (fieldNames :: IndexDocumentsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {fieldNames = a} :: IndexDocumentsResponse)
{-# DEPRECATED idrsFieldNames "Use generic-lens or generic-optics with 'fieldNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idrsResponseStatus :: Lens.Lens' IndexDocumentsResponse Lude.Int
idrsResponseStatus = Lens.lens (responseStatus :: IndexDocumentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: IndexDocumentsResponse)
{-# DEPRECATED idrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
