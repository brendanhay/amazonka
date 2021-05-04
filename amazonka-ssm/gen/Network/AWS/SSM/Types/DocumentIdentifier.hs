{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.DocumentIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentIdentifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.ReviewStatus
import Network.AWS.SSM.Types.Tag

-- | Describes the name of a Systems Manager document.
--
-- /See:/ 'newDocumentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
  { -- | The document type.
    documentType :: Prelude.Maybe DocumentType,
    -- | The operating system platform.
    platformTypes :: Prelude.Maybe [PlatformType],
    -- | The target type which defines the kinds of resources the document can
    -- run on. For example, \/AWS::EC2::Instance. For a list of valid resource
    -- types, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
    -- in the /AWS CloudFormation User Guide/.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document, and cannot be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The user in your organization who created the document.
    author :: Prelude.Maybe Prelude.Text,
    -- | The name of the Systems Manager document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The tags, or metadata, that have been applied to the document.
    tags :: Prelude.Maybe [Tag],
    -- | The AWS user account that created the document.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The current status of a document review.
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | The schema version.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DocumentIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentType', 'documentIdentifier_documentType' - The document type.
--
-- 'platformTypes', 'documentIdentifier_platformTypes' - The operating system platform.
--
-- 'targetType', 'documentIdentifier_targetType' - The target type which defines the kinds of resources the document can
-- run on. For example, \/AWS::EC2::Instance. For a list of valid resource
-- types, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
--
-- 'requires', 'documentIdentifier_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'versionName', 'documentIdentifier_versionName' - An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and cannot be changed.
--
-- 'author', 'documentIdentifier_author' - The user in your organization who created the document.
--
-- 'name', 'documentIdentifier_name' - The name of the Systems Manager document.
--
-- 'documentFormat', 'documentIdentifier_documentFormat' - The document format, either JSON or YAML.
--
-- 'tags', 'documentIdentifier_tags' - The tags, or metadata, that have been applied to the document.
--
-- 'owner', 'documentIdentifier_owner' - The AWS user account that created the document.
--
-- 'reviewStatus', 'documentIdentifier_reviewStatus' - The current status of a document review.
--
-- 'schemaVersion', 'documentIdentifier_schemaVersion' - The schema version.
--
-- 'documentVersion', 'documentIdentifier_documentVersion' - The document version.
newDocumentIdentifier ::
  DocumentIdentifier
newDocumentIdentifier =
  DocumentIdentifier'
    { documentType = Prelude.Nothing,
      platformTypes = Prelude.Nothing,
      targetType = Prelude.Nothing,
      requires = Prelude.Nothing,
      versionName = Prelude.Nothing,
      author = Prelude.Nothing,
      name = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      tags = Prelude.Nothing,
      owner = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      documentVersion = Prelude.Nothing
    }

-- | The document type.
documentIdentifier_documentType :: Lens.Lens' DocumentIdentifier (Prelude.Maybe DocumentType)
documentIdentifier_documentType = Lens.lens (\DocumentIdentifier' {documentType} -> documentType) (\s@DocumentIdentifier' {} a -> s {documentType = a} :: DocumentIdentifier)

-- | The operating system platform.
documentIdentifier_platformTypes :: Lens.Lens' DocumentIdentifier (Prelude.Maybe [PlatformType])
documentIdentifier_platformTypes = Lens.lens (\DocumentIdentifier' {platformTypes} -> platformTypes) (\s@DocumentIdentifier' {} a -> s {platformTypes = a} :: DocumentIdentifier) Prelude.. Lens.mapping Prelude._Coerce

-- | The target type which defines the kinds of resources the document can
-- run on. For example, \/AWS::EC2::Instance. For a list of valid resource
-- types, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
documentIdentifier_targetType :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_targetType = Lens.lens (\DocumentIdentifier' {targetType} -> targetType) (\s@DocumentIdentifier' {} a -> s {targetType = a} :: DocumentIdentifier)

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
documentIdentifier_requires :: Lens.Lens' DocumentIdentifier (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
documentIdentifier_requires = Lens.lens (\DocumentIdentifier' {requires} -> requires) (\s@DocumentIdentifier' {} a -> s {requires = a} :: DocumentIdentifier) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and cannot be changed.
documentIdentifier_versionName :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_versionName = Lens.lens (\DocumentIdentifier' {versionName} -> versionName) (\s@DocumentIdentifier' {} a -> s {versionName = a} :: DocumentIdentifier)

-- | The user in your organization who created the document.
documentIdentifier_author :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_author = Lens.lens (\DocumentIdentifier' {author} -> author) (\s@DocumentIdentifier' {} a -> s {author = a} :: DocumentIdentifier)

-- | The name of the Systems Manager document.
documentIdentifier_name :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_name = Lens.lens (\DocumentIdentifier' {name} -> name) (\s@DocumentIdentifier' {} a -> s {name = a} :: DocumentIdentifier)

-- | The document format, either JSON or YAML.
documentIdentifier_documentFormat :: Lens.Lens' DocumentIdentifier (Prelude.Maybe DocumentFormat)
documentIdentifier_documentFormat = Lens.lens (\DocumentIdentifier' {documentFormat} -> documentFormat) (\s@DocumentIdentifier' {} a -> s {documentFormat = a} :: DocumentIdentifier)

-- | The tags, or metadata, that have been applied to the document.
documentIdentifier_tags :: Lens.Lens' DocumentIdentifier (Prelude.Maybe [Tag])
documentIdentifier_tags = Lens.lens (\DocumentIdentifier' {tags} -> tags) (\s@DocumentIdentifier' {} a -> s {tags = a} :: DocumentIdentifier) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS user account that created the document.
documentIdentifier_owner :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_owner = Lens.lens (\DocumentIdentifier' {owner} -> owner) (\s@DocumentIdentifier' {} a -> s {owner = a} :: DocumentIdentifier)

-- | The current status of a document review.
documentIdentifier_reviewStatus :: Lens.Lens' DocumentIdentifier (Prelude.Maybe ReviewStatus)
documentIdentifier_reviewStatus = Lens.lens (\DocumentIdentifier' {reviewStatus} -> reviewStatus) (\s@DocumentIdentifier' {} a -> s {reviewStatus = a} :: DocumentIdentifier)

-- | The schema version.
documentIdentifier_schemaVersion :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_schemaVersion = Lens.lens (\DocumentIdentifier' {schemaVersion} -> schemaVersion) (\s@DocumentIdentifier' {} a -> s {schemaVersion = a} :: DocumentIdentifier)

-- | The document version.
documentIdentifier_documentVersion :: Lens.Lens' DocumentIdentifier (Prelude.Maybe Prelude.Text)
documentIdentifier_documentVersion = Lens.lens (\DocumentIdentifier' {documentVersion} -> documentVersion) (\s@DocumentIdentifier' {} a -> s {documentVersion = a} :: DocumentIdentifier)

instance Prelude.FromJSON DocumentIdentifier where
  parseJSON =
    Prelude.withObject
      "DocumentIdentifier"
      ( \x ->
          DocumentIdentifier'
            Prelude.<$> (x Prelude..:? "DocumentType")
            Prelude.<*> ( x Prelude..:? "PlatformTypes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "TargetType")
            Prelude.<*> (x Prelude..:? "Requires")
            Prelude.<*> (x Prelude..:? "VersionName")
            Prelude.<*> (x Prelude..:? "Author")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DocumentFormat")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Owner")
            Prelude.<*> (x Prelude..:? "ReviewStatus")
            Prelude.<*> (x Prelude..:? "SchemaVersion")
            Prelude.<*> (x Prelude..:? "DocumentVersion")
      )

instance Prelude.Hashable DocumentIdentifier

instance Prelude.NFData DocumentIdentifier
