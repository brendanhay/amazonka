{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @'IndexField' @ from the search domain. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteIndexField
  ( -- * Creating a request
    DeleteIndexField (..),
    mkDeleteIndexField,

    -- ** Request lenses
    diffDomainName,
    diffIndexFieldName,

    -- * Destructuring the response
    DeleteIndexFieldResponse (..),
    mkDeleteIndexFieldResponse,

    -- ** Response lenses
    difrsIndexField,
    difrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteIndexField' @ operation. Specifies the name of the domain you want to update and the name of the index field you want to delete.
--
-- /See:/ 'mkDeleteIndexField' smart constructor.
data DeleteIndexField = DeleteIndexField'
  { domainName :: Lude.Text,
    -- | The name of the index field your want to remove from the domain's indexing options.
    indexFieldName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIndexField' with the minimum fields required to make a request.
--
-- * 'domainName' -
-- * 'indexFieldName' - The name of the index field your want to remove from the domain's indexing options.
mkDeleteIndexField ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'indexFieldName'
  Lude.Text ->
  DeleteIndexField
mkDeleteIndexField pDomainName_ pIndexFieldName_ =
  DeleteIndexField'
    { domainName = pDomainName_,
      indexFieldName = pIndexFieldName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diffDomainName :: Lens.Lens' DeleteIndexField Lude.Text
diffDomainName = Lens.lens (domainName :: DeleteIndexField -> Lude.Text) (\s a -> s {domainName = a} :: DeleteIndexField)
{-# DEPRECATED diffDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the index field your want to remove from the domain's indexing options.
--
-- /Note:/ Consider using 'indexFieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diffIndexFieldName :: Lens.Lens' DeleteIndexField Lude.Text
diffIndexFieldName = Lens.lens (indexFieldName :: DeleteIndexField -> Lude.Text) (\s a -> s {indexFieldName = a} :: DeleteIndexField)
{-# DEPRECATED diffIndexFieldName "Use generic-lens or generic-optics with 'indexFieldName' instead." #-}

instance Lude.AWSRequest DeleteIndexField where
  type Rs DeleteIndexField = DeleteIndexFieldResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DeleteIndexFieldResult"
      ( \s h x ->
          DeleteIndexFieldResponse'
            Lude.<$> (x Lude..@ "IndexField") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIndexField where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteIndexField where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIndexField where
  toQuery DeleteIndexField' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteIndexField" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "IndexFieldName" Lude.=: indexFieldName
      ]

-- | The result of a @'DeleteIndexField' @ request.
--
-- /See:/ 'mkDeleteIndexFieldResponse' smart constructor.
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
  { -- | The status of the index field being deleted.
    indexField :: IndexFieldStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIndexFieldResponse' with the minimum fields required to make a request.
--
-- * 'indexField' - The status of the index field being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteIndexFieldResponse ::
  -- | 'indexField'
  IndexFieldStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIndexFieldResponse
mkDeleteIndexFieldResponse pIndexField_ pResponseStatus_ =
  DeleteIndexFieldResponse'
    { indexField = pIndexField_,
      responseStatus = pResponseStatus_
    }

-- | The status of the index field being deleted.
--
-- /Note:/ Consider using 'indexField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsIndexField :: Lens.Lens' DeleteIndexFieldResponse IndexFieldStatus
difrsIndexField = Lens.lens (indexField :: DeleteIndexFieldResponse -> IndexFieldStatus) (\s a -> s {indexField = a} :: DeleteIndexFieldResponse)
{-# DEPRECATED difrsIndexField "Use generic-lens or generic-optics with 'indexField' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsResponseStatus :: Lens.Lens' DeleteIndexFieldResponse Lude.Int
difrsResponseStatus = Lens.lens (responseStatus :: DeleteIndexFieldResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIndexFieldResponse)
{-# DEPRECATED difrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
