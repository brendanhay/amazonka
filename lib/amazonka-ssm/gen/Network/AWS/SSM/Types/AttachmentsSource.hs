-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentsSource
  ( AttachmentsSource (..),

    -- * Smart constructor
    mkAttachmentsSource,

    -- * Lenses
    aValues,
    aKey,
    aName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AttachmentsSourceKey

-- | Identifying information about a document attachment, including the file name and a key-value pair that identifies the location of an attachment to a document.
--
-- /See:/ 'mkAttachmentsSource' smart constructor.
data AttachmentsSource = AttachmentsSource'
  { values ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    key :: Lude.Maybe AttachmentsSourceKey,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachmentsSource' with the minimum fields required to make a request.
--
-- * 'key' - The key of a key-value pair that identifies the location of an attachment to a document.
-- * 'name' - The name of the document attachment file.
-- * 'values' - The value of a key-value pair that identifies the location of an attachment to a document. The format for __Value__ depends on the type of key you specify.
--
--
--     * For the key /SourceUrl/ , the value is an S3 bucket location. For example:
-- @"Values": [ "s3://doc-example-bucket/my-folder" ]@
--
--
--     * For the key /S3FileUrl/ , the value is a file in an S3 bucket. For example:
-- @"Values": [ "s3://doc-example-bucket/my-folder/my-file.py" ]@
--
--
--     * For the key /AttachmentReference/ , the value is constructed from the name of another SSM document in your account, a version number of that document, and a file attached to that document version that you want to reuse. For example:
-- @"Values": [ "MyOtherDocument/3/my-other-file.py" ]@
-- However, if the SSM document is shared with you from another account, the full SSM document ARN must be specified instead of the document name only. For example:
-- @"Values": [ "arn:aws:ssm:us-east-2:111122223333:document/OtherAccountDocument/3/their-file.py" ]@
mkAttachmentsSource ::
  AttachmentsSource
mkAttachmentsSource =
  AttachmentsSource'
    { values = Lude.Nothing,
      key = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The value of a key-value pair that identifies the location of an attachment to a document. The format for __Value__ depends on the type of key you specify.
--
--
--     * For the key /SourceUrl/ , the value is an S3 bucket location. For example:
-- @"Values": [ "s3://doc-example-bucket/my-folder" ]@
--
--
--     * For the key /S3FileUrl/ , the value is a file in an S3 bucket. For example:
-- @"Values": [ "s3://doc-example-bucket/my-folder/my-file.py" ]@
--
--
--     * For the key /AttachmentReference/ , the value is constructed from the name of another SSM document in your account, a version number of that document, and a file attached to that document version that you want to reuse. For example:
-- @"Values": [ "MyOtherDocument/3/my-other-file.py" ]@
-- However, if the SSM document is shared with you from another account, the full SSM document ARN must be specified instead of the document name only. For example:
-- @"Values": [ "arn:aws:ssm:us-east-2:111122223333:document/OtherAccountDocument/3/their-file.py" ]@
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValues :: Lens.Lens' AttachmentsSource (Lude.Maybe (Lude.NonEmpty Lude.Text))
aValues = Lens.lens (values :: AttachmentsSource -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {values = a} :: AttachmentsSource)
{-# DEPRECATED aValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The key of a key-value pair that identifies the location of an attachment to a document.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKey :: Lens.Lens' AttachmentsSource (Lude.Maybe AttachmentsSourceKey)
aKey = Lens.lens (key :: AttachmentsSource -> Lude.Maybe AttachmentsSourceKey) (\s a -> s {key = a} :: AttachmentsSource)
{-# DEPRECATED aKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The name of the document attachment file.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' AttachmentsSource (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: AttachmentsSource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AttachmentsSource)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON AttachmentsSource where
  toJSON AttachmentsSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("Key" Lude..=) Lude.<$> key,
            ("Name" Lude..=) Lude.<$> name
          ]
      )
