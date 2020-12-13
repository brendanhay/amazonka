{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Systems Manager document.
module Network.AWS.SSM.DescribeDocument
  ( -- * Creating a request
    DescribeDocument (..),
    mkDescribeDocument,

    -- ** Request lenses
    ddgVersionName,
    ddgName,
    ddgDocumentVersion,

    -- * Destructuring the response
    DescribeDocumentResponse (..),
    mkDescribeDocumentResponse,

    -- ** Response lenses
    drsDocument,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeDocument' smart constructor.
data DescribeDocument = DescribeDocument'
  { -- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
    versionName :: Lude.Maybe Lude.Text,
    -- | The name of the Systems Manager document.
    name :: Lude.Text,
    -- | The document version for which you want information. Can be a specific version or the default version.
    documentVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocument' with the minimum fields required to make a request.
--
-- * 'versionName' - An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
-- * 'name' - The name of the Systems Manager document.
-- * 'documentVersion' - The document version for which you want information. Can be a specific version or the default version.
mkDescribeDocument ::
  -- | 'name'
  Lude.Text ->
  DescribeDocument
mkDescribeDocument pName_ =
  DescribeDocument'
    { versionName = Lude.Nothing,
      name = pName_,
      documentVersion = Lude.Nothing
    }

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgVersionName :: Lens.Lens' DescribeDocument (Lude.Maybe Lude.Text)
ddgVersionName = Lens.lens (versionName :: DescribeDocument -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: DescribeDocument)
{-# DEPRECATED ddgVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgName :: Lens.Lens' DescribeDocument Lude.Text
ddgName = Lens.lens (name :: DescribeDocument -> Lude.Text) (\s a -> s {name = a} :: DescribeDocument)
{-# DEPRECATED ddgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The document version for which you want information. Can be a specific version or the default version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgDocumentVersion :: Lens.Lens' DescribeDocument (Lude.Maybe Lude.Text)
ddgDocumentVersion = Lens.lens (documentVersion :: DescribeDocument -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: DescribeDocument)
{-# DEPRECATED ddgDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

instance Lude.AWSRequest DescribeDocument where
  type Rs DescribeDocument = DescribeDocumentResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDocumentResponse'
            Lude.<$> (x Lude..?> "Document") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDocument where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeDocument" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDocument where
  toJSON DescribeDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionName" Lude..=) Lude.<$> versionName,
            Lude.Just ("Name" Lude..= name),
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion
          ]
      )

instance Lude.ToPath DescribeDocument where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDocumentResponse' smart constructor.
data DescribeDocumentResponse = DescribeDocumentResponse'
  { -- | Information about the Systems Manager document.
    document :: Lude.Maybe DocumentDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentResponse' with the minimum fields required to make a request.
--
-- * 'document' - Information about the Systems Manager document.
-- * 'responseStatus' - The response status code.
mkDescribeDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDocumentResponse
mkDescribeDocumentResponse pResponseStatus_ =
  DescribeDocumentResponse'
    { document = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Systems Manager document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDocument :: Lens.Lens' DescribeDocumentResponse (Lude.Maybe DocumentDescription)
drsDocument = Lens.lens (document :: DescribeDocumentResponse -> Lude.Maybe DocumentDescription) (\s a -> s {document = a} :: DescribeDocumentResponse)
{-# DEPRECATED drsDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeDocumentResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDocumentResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
