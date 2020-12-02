{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceIdentifierSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceIdentifierSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the target resources of a specific type in your import template (for example, all @AWS::S3::Bucket@ resources) and the properties you can provide during the import to identify resources of that type.
--
--
--
-- /See:/ 'resourceIdentifierSummary' smart constructor.
data ResourceIdentifierSummary = ResourceIdentifierSummary'
  { _risResourceType ::
      !(Maybe Text),
    _risLogicalResourceIds ::
      !(Maybe (List1 Text)),
    _risResourceIdentifiers ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceIdentifierSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'risResourceType' - The template resource type of the target resources, such as @AWS::S3::Bucket@ .
--
-- * 'risLogicalResourceIds' - The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
--
-- * 'risResourceIdentifiers' - The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
resourceIdentifierSummary ::
  ResourceIdentifierSummary
resourceIdentifierSummary =
  ResourceIdentifierSummary'
    { _risResourceType = Nothing,
      _risLogicalResourceIds = Nothing,
      _risResourceIdentifiers = Nothing
    }

-- | The template resource type of the target resources, such as @AWS::S3::Bucket@ .
risResourceType :: Lens' ResourceIdentifierSummary (Maybe Text)
risResourceType = lens _risResourceType (\s a -> s {_risResourceType = a})

-- | The logical IDs of the target resources of the specified @ResourceType@ , as defined in the import template.
risLogicalResourceIds :: Lens' ResourceIdentifierSummary (Maybe (NonEmpty Text))
risLogicalResourceIds = lens _risLogicalResourceIds (\s a -> s {_risLogicalResourceIds = a}) . mapping _List1

-- | The resource properties you can provide during the import to identify your target resources. For example, @BucketName@ is a possible identifier property for @AWS::S3::Bucket@ resources.
risResourceIdentifiers :: Lens' ResourceIdentifierSummary [Text]
risResourceIdentifiers = lens _risResourceIdentifiers (\s a -> s {_risResourceIdentifiers = a}) . _Default . _Coerce

instance FromXML ResourceIdentifierSummary where
  parseXML x =
    ResourceIdentifierSummary'
      <$> (x .@? "ResourceType")
      <*> ( x .@? "LogicalResourceIds" .!@ mempty
              >>= may (parseXMLList1 "member")
          )
      <*> ( x .@? "ResourceIdentifiers" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable ResourceIdentifierSummary

instance NFData ResourceIdentifierSummary
