{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more values for an SSM document.
module Network.AWS.SSM.UpdateDocument
  ( -- * Creating a request
    UpdateDocument (..),
    mkUpdateDocument,

    -- ** Request lenses
    udAttachments,
    udVersionName,
    udTargetType,
    udDocumentFormat,
    udDocumentVersion,
    udContent,
    udName,

    -- * Destructuring the response
    UpdateDocumentResponse (..),
    mkUpdateDocumentResponse,

    -- ** Response lenses
    udrsDocumentDescription,
    udrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { attachments ::
      Lude.Maybe [AttachmentsSource],
    versionName :: Lude.Maybe Lude.Text,
    targetType :: Lude.Maybe Lude.Text,
    documentFormat :: Lude.Maybe DocumentFormat,
    documentVersion :: Lude.Maybe Lude.Text,
    content :: Lude.Text,
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

-- | Creates a value of 'UpdateDocument' with the minimum fields required to make a request.
--
-- * 'attachments' - A list of key and value pairs that describe attachments to a version of a document.
-- * 'content' - A valid JSON or YAML string.
-- * 'documentFormat' - Specify the document format for the new document version. Systems Manager supports JSON and YAML documents. JSON is the default format.
-- * 'documentVersion' - (Required) The latest version of the document that you want to update. The latest document version can be specified using the $LATEST variable or by the version number. Updating a previous version of a document is not supported.
-- * 'name' - The name of the document that you want to update.
-- * 'targetType' - Specify a new target type for the document.
-- * 'versionName' - An optional field specifying the version of the artifact you are updating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
mkUpdateDocument ::
  -- | 'content'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  UpdateDocument
mkUpdateDocument pContent_ pName_ =
  UpdateDocument'
    { attachments = Lude.Nothing,
      versionName = Lude.Nothing,
      targetType = Lude.Nothing,
      documentFormat = Lude.Nothing,
      documentVersion = Lude.Nothing,
      content = pContent_,
      name = pName_
    }

-- | A list of key and value pairs that describe attachments to a version of a document.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udAttachments :: Lens.Lens' UpdateDocument (Lude.Maybe [AttachmentsSource])
udAttachments = Lens.lens (attachments :: UpdateDocument -> Lude.Maybe [AttachmentsSource]) (\s a -> s {attachments = a} :: UpdateDocument)
{-# DEPRECATED udAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | An optional field specifying the version of the artifact you are updating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udVersionName :: Lens.Lens' UpdateDocument (Lude.Maybe Lude.Text)
udVersionName = Lens.lens (versionName :: UpdateDocument -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: UpdateDocument)
{-# DEPRECATED udVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | Specify a new target type for the document.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udTargetType :: Lens.Lens' UpdateDocument (Lude.Maybe Lude.Text)
udTargetType = Lens.lens (targetType :: UpdateDocument -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: UpdateDocument)
{-# DEPRECATED udTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | Specify the document format for the new document version. Systems Manager supports JSON and YAML documents. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocumentFormat :: Lens.Lens' UpdateDocument (Lude.Maybe DocumentFormat)
udDocumentFormat = Lens.lens (documentFormat :: UpdateDocument -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: UpdateDocument)
{-# DEPRECATED udDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | (Required) The latest version of the document that you want to update. The latest document version can be specified using the $LATEST variable or by the version number. Updating a previous version of a document is not supported.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocumentVersion :: Lens.Lens' UpdateDocument (Lude.Maybe Lude.Text)
udDocumentVersion = Lens.lens (documentVersion :: UpdateDocument -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: UpdateDocument)
{-# DEPRECATED udDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | A valid JSON or YAML string.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udContent :: Lens.Lens' UpdateDocument Lude.Text
udContent = Lens.lens (content :: UpdateDocument -> Lude.Text) (\s a -> s {content = a} :: UpdateDocument)
{-# DEPRECATED udContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The name of the document that you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDocument Lude.Text
udName = Lens.lens (name :: UpdateDocument -> Lude.Text) (\s a -> s {name = a} :: UpdateDocument)
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateDocument where
  type Rs UpdateDocument = UpdateDocumentResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDocumentResponse'
            Lude.<$> (x Lude..?> "DocumentDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDocument where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateDocument" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDocument where
  toJSON UpdateDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Attachments" Lude..=) Lude.<$> attachments,
            ("VersionName" Lude..=) Lude.<$> versionName,
            ("TargetType" Lude..=) Lude.<$> targetType,
            ("DocumentFormat" Lude..=) Lude.<$> documentFormat,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            Lude.Just ("Content" Lude..= content),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateDocument where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  { documentDescription ::
      Lude.Maybe DocumentDescription,
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

-- | Creates a value of 'UpdateDocumentResponse' with the minimum fields required to make a request.
--
-- * 'documentDescription' - A description of the document that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDocumentResponse
mkUpdateDocumentResponse pResponseStatus_ =
  UpdateDocumentResponse'
    { documentDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the document that was updated.
--
-- /Note:/ Consider using 'documentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsDocumentDescription :: Lens.Lens' UpdateDocumentResponse (Lude.Maybe DocumentDescription)
udrsDocumentDescription = Lens.lens (documentDescription :: UpdateDocumentResponse -> Lude.Maybe DocumentDescription) (\s a -> s {documentDescription = a} :: UpdateDocumentResponse)
{-# DEPRECATED udrsDocumentDescription "Use generic-lens or generic-optics with 'documentDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDocumentResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDocumentResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
