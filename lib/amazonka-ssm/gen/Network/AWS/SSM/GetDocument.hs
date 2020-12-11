{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Systems Manager document.
module Network.AWS.SSM.GetDocument
  ( -- * Creating a request
    GetDocument (..),
    mkGetDocument,

    -- ** Request lenses
    gdVersionName,
    gdDocumentFormat,
    gdDocumentVersion,
    gdName,

    -- * Destructuring the response
    GetDocumentResponse (..),
    mkGetDocumentResponse,

    -- ** Response lenses
    gdrsStatus,
    gdrsDocumentType,
    gdrsVersionName,
    gdrsAttachmentsContent,
    gdrsContent,
    gdrsDocumentFormat,
    gdrsName,
    gdrsDocumentVersion,
    gdrsStatusInformation,
    gdrsRequires,
    gdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetDocument' smart constructor.
data GetDocument = GetDocument'
  { versionName ::
      Lude.Maybe Lude.Text,
    documentFormat :: Lude.Maybe DocumentFormat,
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

-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- * 'documentFormat' - Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
-- * 'documentVersion' - The document version for which you want information.
-- * 'name' - The name of the Systems Manager document.
-- * 'versionName' - An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
mkGetDocument ::
  -- | 'name'
  Lude.Text ->
  GetDocument
mkGetDocument pName_ =
  GetDocument'
    { versionName = Lude.Nothing,
      documentFormat = Lude.Nothing,
      documentVersion = Lude.Nothing,
      name = pName_
    }

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdVersionName :: Lens.Lens' GetDocument (Lude.Maybe Lude.Text)
gdVersionName = Lens.lens (versionName :: GetDocument -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: GetDocument)
{-# DEPRECATED gdVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentFormat :: Lens.Lens' GetDocument (Lude.Maybe DocumentFormat)
gdDocumentFormat = Lens.lens (documentFormat :: GetDocument -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: GetDocument)
{-# DEPRECATED gdDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The document version for which you want information.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentVersion :: Lens.Lens' GetDocument (Lude.Maybe Lude.Text)
gdDocumentVersion = Lens.lens (documentVersion :: GetDocument -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: GetDocument)
{-# DEPRECATED gdDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdName :: Lens.Lens' GetDocument Lude.Text
gdName = Lens.lens (name :: GetDocument -> Lude.Text) (\s a -> s {name = a} :: GetDocument)
{-# DEPRECATED gdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetDocument where
  type Rs GetDocument = GetDocumentResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "DocumentType")
            Lude.<*> (x Lude..?> "VersionName")
            Lude.<*> (x Lude..?> "AttachmentsContent" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Content")
            Lude.<*> (x Lude..?> "DocumentFormat")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "DocumentVersion")
            Lude.<*> (x Lude..?> "StatusInformation")
            Lude.<*> (x Lude..?> "Requires")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDocument where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetDocument" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDocument where
  toJSON GetDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionName" Lude..=) Lude.<$> versionName,
            ("DocumentFormat" Lude..=) Lude.<$> documentFormat,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath GetDocument where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { status ::
      Lude.Maybe DocumentStatus,
    documentType :: Lude.Maybe DocumentType,
    versionName :: Lude.Maybe Lude.Text,
    attachmentsContent ::
      Lude.Maybe [AttachmentContent],
    content :: Lude.Maybe Lude.Text,
    documentFormat :: Lude.Maybe DocumentFormat,
    name :: Lude.Maybe Lude.Text,
    documentVersion :: Lude.Maybe Lude.Text,
    statusInformation :: Lude.Maybe Lude.Text,
    requires ::
      Lude.Maybe (Lude.NonEmpty DocumentRequires),
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

-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- * 'attachmentsContent' - A description of the document attachments, including names, locations, sizes, and so on.
-- * 'content' - The contents of the Systems Manager document.
-- * 'documentFormat' - The document format, either JSON or YAML.
-- * 'documentType' - The document type.
-- * 'documentVersion' - The document version.
-- * 'name' - The name of the Systems Manager document.
-- * 'requires' - A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
-- * 'statusInformation' - A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
-- * 'versionName' - The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
mkGetDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDocumentResponse
mkGetDocumentResponse pResponseStatus_ =
  GetDocumentResponse'
    { status = Lude.Nothing,
      documentType = Lude.Nothing,
      versionName = Lude.Nothing,
      attachmentsContent = Lude.Nothing,
      content = Lude.Nothing,
      documentFormat = Lude.Nothing,
      name = Lude.Nothing,
      documentVersion = Lude.Nothing,
      statusInformation = Lude.Nothing,
      requires = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsStatus :: Lens.Lens' GetDocumentResponse (Lude.Maybe DocumentStatus)
gdrsStatus = Lens.lens (status :: GetDocumentResponse -> Lude.Maybe DocumentStatus) (\s a -> s {status = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The document type.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDocumentType :: Lens.Lens' GetDocumentResponse (Lude.Maybe DocumentType)
gdrsDocumentType = Lens.lens (documentType :: GetDocumentResponse -> Lude.Maybe DocumentType) (\s a -> s {documentType = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsVersionName :: Lens.Lens' GetDocumentResponse (Lude.Maybe Lude.Text)
gdrsVersionName = Lens.lens (versionName :: GetDocumentResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | A description of the document attachments, including names, locations, sizes, and so on.
--
-- /Note:/ Consider using 'attachmentsContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsAttachmentsContent :: Lens.Lens' GetDocumentResponse (Lude.Maybe [AttachmentContent])
gdrsAttachmentsContent = Lens.lens (attachmentsContent :: GetDocumentResponse -> Lude.Maybe [AttachmentContent]) (\s a -> s {attachmentsContent = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsAttachmentsContent "Use generic-lens or generic-optics with 'attachmentsContent' instead." #-}

-- | The contents of the Systems Manager document.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsContent :: Lens.Lens' GetDocumentResponse (Lude.Maybe Lude.Text)
gdrsContent = Lens.lens (content :: GetDocumentResponse -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDocumentFormat :: Lens.Lens' GetDocumentResponse (Lude.Maybe DocumentFormat)
gdrsDocumentFormat = Lens.lens (documentFormat :: GetDocumentResponse -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsName :: Lens.Lens' GetDocumentResponse (Lude.Maybe Lude.Text)
gdrsName = Lens.lens (name :: GetDocumentResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDocumentVersion :: Lens.Lens' GetDocumentResponse (Lude.Maybe Lude.Text)
gdrsDocumentVersion = Lens.lens (documentVersion :: GetDocumentResponse -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsStatusInformation :: Lens.Lens' GetDocumentResponse (Lude.Maybe Lude.Text)
gdrsStatusInformation = Lens.lens (statusInformation :: GetDocumentResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusInformation = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsStatusInformation "Use generic-lens or generic-optics with 'statusInformation' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsRequires :: Lens.Lens' GetDocumentResponse (Lude.Maybe (Lude.NonEmpty DocumentRequires))
gdrsRequires = Lens.lens (requires :: GetDocumentResponse -> Lude.Maybe (Lude.NonEmpty DocumentRequires)) (\s a -> s {requires = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDocumentResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
