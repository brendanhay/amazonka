{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AttachmentsSource
  ( AttachmentsSource (..)
  -- * Smart constructor
  , mkAttachmentsSource
  -- * Lenses
  , asfKey
  , asfName
  , asfValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttachmentIdentifier as Types
import qualified Network.AWS.SSM.Types.AttachmentsSourceKey as Types
import qualified Network.AWS.SSM.Types.AttachmentsSourceValue as Types

-- | Identifying information about a document attachment, including the file name and a key-value pair that identifies the location of an attachment to a document.
--
-- /See:/ 'mkAttachmentsSource' smart constructor.
data AttachmentsSource = AttachmentsSource'
  { key :: Core.Maybe Types.AttachmentsSourceKey
    -- ^ The key of a key-value pair that identifies the location of an attachment to a document.
  , name :: Core.Maybe Types.AttachmentIdentifier
    -- ^ The name of the document attachment file.
  , values :: Core.Maybe (Core.NonEmpty Types.AttachmentsSourceValue)
    -- ^ The value of a key-value pair that identifies the location of an attachment to a document. The format for __Value__ depends on the type of key you specify.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachmentsSource' value with any optional fields omitted.
mkAttachmentsSource
    :: AttachmentsSource
mkAttachmentsSource
  = AttachmentsSource'{key = Core.Nothing, name = Core.Nothing,
                       values = Core.Nothing}

-- | The key of a key-value pair that identifies the location of an attachment to a document.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asfKey :: Lens.Lens' AttachmentsSource (Core.Maybe Types.AttachmentsSourceKey)
asfKey = Lens.field @"key"
{-# INLINEABLE asfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The name of the document attachment file.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asfName :: Lens.Lens' AttachmentsSource (Core.Maybe Types.AttachmentIdentifier)
asfName = Lens.field @"name"
{-# INLINEABLE asfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
asfValues :: Lens.Lens' AttachmentsSource (Core.Maybe (Core.NonEmpty Types.AttachmentsSourceValue))
asfValues = Lens.field @"values"
{-# INLINEABLE asfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON AttachmentsSource where
        toJSON AttachmentsSource{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Name" Core..=) Core.<$> name,
                  ("Values" Core..=) Core.<$> values])
