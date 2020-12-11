{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to the document.
--
-- Before you delete the document, we recommend that you use 'DeleteAssociation' to disassociate all instances that are associated with the document.
module Network.AWS.SSM.DeleteDocument
  ( -- * Creating a request
    DeleteDocument (..),
    mkDeleteDocument,

    -- ** Request lenses
    dddVersionName,
    dddForce,
    dddDocumentVersion,
    dddName,

    -- * Destructuring the response
    DeleteDocumentResponse (..),
    mkDeleteDocumentResponse,

    -- ** Response lenses
    ddrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { versionName ::
      Lude.Maybe Lude.Text,
    force :: Lude.Maybe Lude.Bool,
    documentVersion :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocument' with the minimum fields required to make a request.
--
-- * 'documentVersion' - The version of the document that you want to delete. If not provided, all versions of the document are deleted.
-- * 'force' - Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
-- * 'name' - The name of the document.
-- * 'versionName' - The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
mkDeleteDocument ::
  -- | 'name'
  Lude.Text ->
  DeleteDocument
mkDeleteDocument pName_ =
  DeleteDocument'
    { versionName = Lude.Nothing,
      force = Lude.Nothing,
      documentVersion = Lude.Nothing,
      name = pName_
    }

-- | The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddVersionName :: Lens.Lens' DeleteDocument (Lude.Maybe Lude.Text)
dddVersionName = Lens.lens (versionName :: DeleteDocument -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: DeleteDocument)
{-# DEPRECATED dddVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddForce :: Lens.Lens' DeleteDocument (Lude.Maybe Lude.Bool)
dddForce = Lens.lens (force :: DeleteDocument -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteDocument)
{-# DEPRECATED dddForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The version of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddDocumentVersion :: Lens.Lens' DeleteDocument (Lude.Maybe Lude.Text)
dddDocumentVersion = Lens.lens (documentVersion :: DeleteDocument -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: DeleteDocument)
{-# DEPRECATED dddDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddName :: Lens.Lens' DeleteDocument Lude.Text
dddName = Lens.lens (name :: DeleteDocument -> Lude.Text) (\s a -> s {name = a} :: DeleteDocument)
{-# DEPRECATED dddName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDocumentResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDocument where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteDocument" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDocument where
  toJSON DeleteDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionName" Lude..=) Lude.<$> versionName,
            ("Force" Lude..=) Lude.<$> force,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DeleteDocument where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDocumentResponse' smart constructor.
newtype DeleteDocumentResponse = DeleteDocumentResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDocumentResponse
mkDeleteDocumentResponse pResponseStatus_ =
  DeleteDocumentResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDocumentResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDocumentResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
