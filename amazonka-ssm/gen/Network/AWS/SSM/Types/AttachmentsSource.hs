{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentsSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentsSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.AttachmentsSourceKey

-- | Identifying information about a document attachment, including the file
-- name and a key-value pair that identifies the location of an attachment
-- to a document.
--
-- /See:/ 'newAttachmentsSource' smart constructor.
data AttachmentsSource = AttachmentsSource'
  { -- | The key of a key-value pair that identifies the location of an
    -- attachment to a document.
    key :: Core.Maybe AttachmentsSourceKey,
    -- | The value of a key-value pair that identifies the location of an
    -- attachment to a document. The format for __Value__ depends on the type
    -- of key you specify.
    --
    -- -   For the key /SourceUrl/, the value is an S3 bucket location. For
    --     example:
    --
    --     @\"Values\": [ \"s3:\/\/doc-example-bucket\/my-folder\" ]@
    --
    -- -   For the key /S3FileUrl/, the value is a file in an S3 bucket. For
    --     example:
    --
    --     @\"Values\": [ \"s3:\/\/doc-example-bucket\/my-folder\/my-file.py\" ]@
    --
    -- -   For the key /AttachmentReference/, the value is constructed from the
    --     name of another SSM document in your account, a version number of
    --     that document, and a file attached to that document version that you
    --     want to reuse. For example:
    --
    --     @\"Values\": [ \"MyOtherDocument\/3\/my-other-file.py\" ]@
    --
    --     However, if the SSM document is shared with you from another
    --     account, the full SSM document ARN must be specified instead of the
    --     document name only. For example:
    --
    --     @\"Values\": [ \"arn:aws:ssm:us-east-2:111122223333:document\/OtherAccountDocument\/3\/their-file.py\" ]@
    values :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The name of the document attachment file.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachmentsSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'attachmentsSource_key' - The key of a key-value pair that identifies the location of an
-- attachment to a document.
--
-- 'values', 'attachmentsSource_values' - The value of a key-value pair that identifies the location of an
-- attachment to a document. The format for __Value__ depends on the type
-- of key you specify.
--
-- -   For the key /SourceUrl/, the value is an S3 bucket location. For
--     example:
--
--     @\"Values\": [ \"s3:\/\/doc-example-bucket\/my-folder\" ]@
--
-- -   For the key /S3FileUrl/, the value is a file in an S3 bucket. For
--     example:
--
--     @\"Values\": [ \"s3:\/\/doc-example-bucket\/my-folder\/my-file.py\" ]@
--
-- -   For the key /AttachmentReference/, the value is constructed from the
--     name of another SSM document in your account, a version number of
--     that document, and a file attached to that document version that you
--     want to reuse. For example:
--
--     @\"Values\": [ \"MyOtherDocument\/3\/my-other-file.py\" ]@
--
--     However, if the SSM document is shared with you from another
--     account, the full SSM document ARN must be specified instead of the
--     document name only. For example:
--
--     @\"Values\": [ \"arn:aws:ssm:us-east-2:111122223333:document\/OtherAccountDocument\/3\/their-file.py\" ]@
--
-- 'name', 'attachmentsSource_name' - The name of the document attachment file.
newAttachmentsSource ::
  AttachmentsSource
newAttachmentsSource =
  AttachmentsSource'
    { key = Core.Nothing,
      values = Core.Nothing,
      name = Core.Nothing
    }

-- | The key of a key-value pair that identifies the location of an
-- attachment to a document.
attachmentsSource_key :: Lens.Lens' AttachmentsSource (Core.Maybe AttachmentsSourceKey)
attachmentsSource_key = Lens.lens (\AttachmentsSource' {key} -> key) (\s@AttachmentsSource' {} a -> s {key = a} :: AttachmentsSource)

-- | The value of a key-value pair that identifies the location of an
-- attachment to a document. The format for __Value__ depends on the type
-- of key you specify.
--
-- -   For the key /SourceUrl/, the value is an S3 bucket location. For
--     example:
--
--     @\"Values\": [ \"s3:\/\/doc-example-bucket\/my-folder\" ]@
--
-- -   For the key /S3FileUrl/, the value is a file in an S3 bucket. For
--     example:
--
--     @\"Values\": [ \"s3:\/\/doc-example-bucket\/my-folder\/my-file.py\" ]@
--
-- -   For the key /AttachmentReference/, the value is constructed from the
--     name of another SSM document in your account, a version number of
--     that document, and a file attached to that document version that you
--     want to reuse. For example:
--
--     @\"Values\": [ \"MyOtherDocument\/3\/my-other-file.py\" ]@
--
--     However, if the SSM document is shared with you from another
--     account, the full SSM document ARN must be specified instead of the
--     document name only. For example:
--
--     @\"Values\": [ \"arn:aws:ssm:us-east-2:111122223333:document\/OtherAccountDocument\/3\/their-file.py\" ]@
attachmentsSource_values :: Lens.Lens' AttachmentsSource (Core.Maybe (Core.NonEmpty Core.Text))
attachmentsSource_values = Lens.lens (\AttachmentsSource' {values} -> values) (\s@AttachmentsSource' {} a -> s {values = a} :: AttachmentsSource) Core.. Lens.mapping Lens._Coerce

-- | The name of the document attachment file.
attachmentsSource_name :: Lens.Lens' AttachmentsSource (Core.Maybe Core.Text)
attachmentsSource_name = Lens.lens (\AttachmentsSource' {name} -> name) (\s@AttachmentsSource' {} a -> s {name = a} :: AttachmentsSource)

instance Core.Hashable AttachmentsSource

instance Core.NFData AttachmentsSource

instance Core.ToJSON AttachmentsSource where
  toJSON AttachmentsSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values,
            ("Name" Core..=) Core.<$> name
          ]
      )
