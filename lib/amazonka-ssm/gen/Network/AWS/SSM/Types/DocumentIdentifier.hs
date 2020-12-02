{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.Tag

-- | Describes the name of a Systems Manager document.
--
--
--
-- /See:/ 'documentIdentifier' smart constructor.
data DocumentIdentifier = DocumentIdentifier'
  { _diDocumentType ::
      !(Maybe DocumentType),
    _diVersionName :: !(Maybe Text),
    _diSchemaVersion :: !(Maybe Text),
    _diTargetType :: !(Maybe Text),
    _diOwner :: !(Maybe Text),
    _diPlatformTypes :: !(Maybe [PlatformType]),
    _diDocumentFormat :: !(Maybe DocumentFormat),
    _diName :: !(Maybe Text),
    _diDocumentVersion :: !(Maybe Text),
    _diRequires :: !(Maybe (List1 DocumentRequires)),
    _diTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDocumentType' - The document type.
--
-- * 'diVersionName' - An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- * 'diSchemaVersion' - The schema version.
--
-- * 'diTargetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
--
-- * 'diOwner' - The AWS user account that created the document.
--
-- * 'diPlatformTypes' - The operating system platform.
--
-- * 'diDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'diName' - The name of the Systems Manager document.
--
-- * 'diDocumentVersion' - The document version.
--
-- * 'diRequires' - A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- * 'diTags' - The tags, or metadata, that have been applied to the document.
documentIdentifier ::
  DocumentIdentifier
documentIdentifier =
  DocumentIdentifier'
    { _diDocumentType = Nothing,
      _diVersionName = Nothing,
      _diSchemaVersion = Nothing,
      _diTargetType = Nothing,
      _diOwner = Nothing,
      _diPlatformTypes = Nothing,
      _diDocumentFormat = Nothing,
      _diName = Nothing,
      _diDocumentVersion = Nothing,
      _diRequires = Nothing,
      _diTags = Nothing
    }

-- | The document type.
diDocumentType :: Lens' DocumentIdentifier (Maybe DocumentType)
diDocumentType = lens _diDocumentType (\s a -> s {_diDocumentType = a})

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
diVersionName :: Lens' DocumentIdentifier (Maybe Text)
diVersionName = lens _diVersionName (\s a -> s {_diVersionName = a})

-- | The schema version.
diSchemaVersion :: Lens' DocumentIdentifier (Maybe Text)
diSchemaVersion = lens _diSchemaVersion (\s a -> s {_diSchemaVersion = a})

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
diTargetType :: Lens' DocumentIdentifier (Maybe Text)
diTargetType = lens _diTargetType (\s a -> s {_diTargetType = a})

-- | The AWS user account that created the document.
diOwner :: Lens' DocumentIdentifier (Maybe Text)
diOwner = lens _diOwner (\s a -> s {_diOwner = a})

-- | The operating system platform.
diPlatformTypes :: Lens' DocumentIdentifier [PlatformType]
diPlatformTypes = lens _diPlatformTypes (\s a -> s {_diPlatformTypes = a}) . _Default . _Coerce

-- | The document format, either JSON or YAML.
diDocumentFormat :: Lens' DocumentIdentifier (Maybe DocumentFormat)
diDocumentFormat = lens _diDocumentFormat (\s a -> s {_diDocumentFormat = a})

-- | The name of the Systems Manager document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\s a -> s {_diName = a})

-- | The document version.
diDocumentVersion :: Lens' DocumentIdentifier (Maybe Text)
diDocumentVersion = lens _diDocumentVersion (\s a -> s {_diDocumentVersion = a})

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
diRequires :: Lens' DocumentIdentifier (Maybe (NonEmpty DocumentRequires))
diRequires = lens _diRequires (\s a -> s {_diRequires = a}) . mapping _List1

-- | The tags, or metadata, that have been applied to the document.
diTags :: Lens' DocumentIdentifier [Tag]
diTags = lens _diTags (\s a -> s {_diTags = a}) . _Default . _Coerce

instance FromJSON DocumentIdentifier where
  parseJSON =
    withObject
      "DocumentIdentifier"
      ( \x ->
          DocumentIdentifier'
            <$> (x .:? "DocumentType")
            <*> (x .:? "VersionName")
            <*> (x .:? "SchemaVersion")
            <*> (x .:? "TargetType")
            <*> (x .:? "Owner")
            <*> (x .:? "PlatformTypes" .!= mempty)
            <*> (x .:? "DocumentFormat")
            <*> (x .:? "Name")
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "Requires")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable DocumentIdentifier

instance NFData DocumentIdentifier
