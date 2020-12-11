-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentVersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentVersionInfo
  ( DocumentVersionInfo (..),

    -- * Smart constructor
    mkDocumentVersionInfo,

    -- * Lenses
    dviStatus,
    dviVersionName,
    dviCreatedDate,
    dviDocumentFormat,
    dviName,
    dviDocumentVersion,
    dviStatusInformation,
    dviIsDefaultVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentStatus

-- | Version information about the document.
--
-- /See:/ 'mkDocumentVersionInfo' smart constructor.
data DocumentVersionInfo = DocumentVersionInfo'
  { status ::
      Lude.Maybe DocumentStatus,
    versionName :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    documentFormat :: Lude.Maybe DocumentFormat,
    name :: Lude.Maybe Lude.Text,
    documentVersion :: Lude.Maybe Lude.Text,
    statusInformation :: Lude.Maybe Lude.Text,
    isDefaultVersion :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentVersionInfo' with the minimum fields required to make a request.
--
-- * 'createdDate' - The date the document was created.
-- * 'documentFormat' - The document format, either JSON or YAML.
-- * 'documentVersion' - The document version.
-- * 'isDefaultVersion' - An identifier for the default version of the document.
-- * 'name' - The document name.
-- * 'status' - The status of the Systems Manager document, such as @Creating@ , @Active@ , @Failed@ , and @Deleting@ .
-- * 'statusInformation' - A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
-- * 'versionName' - The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
mkDocumentVersionInfo ::
  DocumentVersionInfo
mkDocumentVersionInfo =
  DocumentVersionInfo'
    { status = Lude.Nothing,
      versionName = Lude.Nothing,
      createdDate = Lude.Nothing,
      documentFormat = Lude.Nothing,
      name = Lude.Nothing,
      documentVersion = Lude.Nothing,
      statusInformation = Lude.Nothing,
      isDefaultVersion = Lude.Nothing
    }

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Failed@ , and @Deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviStatus :: Lens.Lens' DocumentVersionInfo (Lude.Maybe DocumentStatus)
dviStatus = Lens.lens (status :: DocumentVersionInfo -> Lude.Maybe DocumentStatus) (\s a -> s {status = a} :: DocumentVersionInfo)
{-# DEPRECATED dviStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviVersionName :: Lens.Lens' DocumentVersionInfo (Lude.Maybe Lude.Text)
dviVersionName = Lens.lens (versionName :: DocumentVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {versionName = a} :: DocumentVersionInfo)
{-# DEPRECATED dviVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The date the document was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviCreatedDate :: Lens.Lens' DocumentVersionInfo (Lude.Maybe Lude.Timestamp)
dviCreatedDate = Lens.lens (createdDate :: DocumentVersionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: DocumentVersionInfo)
{-# DEPRECATED dviCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviDocumentFormat :: Lens.Lens' DocumentVersionInfo (Lude.Maybe DocumentFormat)
dviDocumentFormat = Lens.lens (documentFormat :: DocumentVersionInfo -> Lude.Maybe DocumentFormat) (\s a -> s {documentFormat = a} :: DocumentVersionInfo)
{-# DEPRECATED dviDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The document name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviName :: Lens.Lens' DocumentVersionInfo (Lude.Maybe Lude.Text)
dviName = Lens.lens (name :: DocumentVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentVersionInfo)
{-# DEPRECATED dviName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviDocumentVersion :: Lens.Lens' DocumentVersionInfo (Lude.Maybe Lude.Text)
dviDocumentVersion = Lens.lens (documentVersion :: DocumentVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: DocumentVersionInfo)
{-# DEPRECATED dviDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviStatusInformation :: Lens.Lens' DocumentVersionInfo (Lude.Maybe Lude.Text)
dviStatusInformation = Lens.lens (statusInformation :: DocumentVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {statusInformation = a} :: DocumentVersionInfo)
{-# DEPRECATED dviStatusInformation "Use generic-lens or generic-optics with 'statusInformation' instead." #-}

-- | An identifier for the default version of the document.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviIsDefaultVersion :: Lens.Lens' DocumentVersionInfo (Lude.Maybe Lude.Bool)
dviIsDefaultVersion = Lens.lens (isDefaultVersion :: DocumentVersionInfo -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: DocumentVersionInfo)
{-# DEPRECATED dviIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

instance Lude.FromJSON DocumentVersionInfo where
  parseJSON =
    Lude.withObject
      "DocumentVersionInfo"
      ( \x ->
          DocumentVersionInfo'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "VersionName")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "DocumentFormat")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "StatusInformation")
            Lude.<*> (x Lude..:? "IsDefaultVersion")
      )
