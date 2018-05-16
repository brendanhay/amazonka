{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.Sum

-- | A resource group.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gDescription :: !(Maybe Text)
  , _gGroupARN    :: !Text
  , _gName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gDescription' - The description of the resource group.
--
-- * 'gGroupARN' - The ARN of a resource group.
--
-- * 'gName' - The name of a resource group.
group'
    :: Text -- ^ 'gGroupARN'
    -> Text -- ^ 'gName'
    -> Group
group' pGroupARN_ pName_ =
  Group' {_gDescription = Nothing, _gGroupARN = pGroupARN_, _gName = pName_}


-- | The description of the resource group.
gDescription :: Lens' Group (Maybe Text)
gDescription = lens _gDescription (\ s a -> s{_gDescription = a})

-- | The ARN of a resource group.
gGroupARN :: Lens' Group Text
gGroupARN = lens _gGroupARN (\ s a -> s{_gGroupARN = a})

-- | The name of a resource group.
gName :: Lens' Group Text
gName = lens _gName (\ s a -> s{_gName = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ x ->
                 Group' <$>
                   (x .:? "Description") <*> (x .: "GroupArn") <*>
                     (x .: "Name"))

instance Hashable Group where

instance NFData Group where

-- | The underlying resource query of a resource group. Resources that match query results are part of the group.
--
--
--
-- /See:/ 'groupQuery' smart constructor.
data GroupQuery = GroupQuery'
  { _gqGroupName     :: !Text
  , _gqResourceQuery :: !ResourceQuery
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqGroupName' - The name of a resource group that is associated with a specific resource query.
--
-- * 'gqResourceQuery' - The resource query which determines which AWS resources are members of the associated resource group.
groupQuery
    :: Text -- ^ 'gqGroupName'
    -> ResourceQuery -- ^ 'gqResourceQuery'
    -> GroupQuery
groupQuery pGroupName_ pResourceQuery_ =
  GroupQuery' {_gqGroupName = pGroupName_, _gqResourceQuery = pResourceQuery_}


-- | The name of a resource group that is associated with a specific resource query.
gqGroupName :: Lens' GroupQuery Text
gqGroupName = lens _gqGroupName (\ s a -> s{_gqGroupName = a})

-- | The resource query which determines which AWS resources are members of the associated resource group.
gqResourceQuery :: Lens' GroupQuery ResourceQuery
gqResourceQuery = lens _gqResourceQuery (\ s a -> s{_gqResourceQuery = a})

instance FromJSON GroupQuery where
        parseJSON
          = withObject "GroupQuery"
              (\ x ->
                 GroupQuery' <$>
                   (x .: "GroupName") <*> (x .: "ResourceQuery"))

instance Hashable GroupQuery where

instance NFData GroupQuery where

-- | The ARN of a resource, and its resource type.
--
--
--
-- /See:/ 'resourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { _riResourceType :: !(Maybe Text)
  , _riResourceARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riResourceType' - The resource type of a resource, such as @AWS::EC2::Instance@ .
--
-- * 'riResourceARN' - The ARN of a resource.
resourceIdentifier
    :: ResourceIdentifier
resourceIdentifier =
  ResourceIdentifier' {_riResourceType = Nothing, _riResourceARN = Nothing}


-- | The resource type of a resource, such as @AWS::EC2::Instance@ .
riResourceType :: Lens' ResourceIdentifier (Maybe Text)
riResourceType = lens _riResourceType (\ s a -> s{_riResourceType = a})

-- | The ARN of a resource.
riResourceARN :: Lens' ResourceIdentifier (Maybe Text)
riResourceARN = lens _riResourceARN (\ s a -> s{_riResourceARN = a})

instance FromJSON ResourceIdentifier where
        parseJSON
          = withObject "ResourceIdentifier"
              (\ x ->
                 ResourceIdentifier' <$>
                   (x .:? "ResourceType") <*> (x .:? "ResourceArn"))

instance Hashable ResourceIdentifier where

instance NFData ResourceIdentifier where

-- | The query that is used to define a resource group or a search for resources.
--
--
--
-- /See:/ 'resourceQuery' smart constructor.
data ResourceQuery = ResourceQuery'
  { _rqType        :: !QueryType
  , _rqSearchQuery :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqType' - The type of the query. The valid value in this release is @TAG_FILTERS_1_0@ . /@TAG_FILTERS_1_0:@ / A JSON syntax that lets you specify a collection of simple tag filters for resource types and tags, as supported by the AWS Tagging API GetResources operation. When more than one element is present, only resources that match all filters are part of the result. If a filter specifies more than one value for a key, a resource matches the filter if its tag value matches any of the specified values.
--
-- * 'rqSearchQuery' - The query that defines a group or a search.
resourceQuery
    :: QueryType -- ^ 'rqType'
    -> Text -- ^ 'rqSearchQuery'
    -> ResourceQuery
resourceQuery pType_ pSearchQuery_ =
  ResourceQuery' {_rqType = pType_, _rqSearchQuery = pSearchQuery_}


-- | The type of the query. The valid value in this release is @TAG_FILTERS_1_0@ . /@TAG_FILTERS_1_0:@ / A JSON syntax that lets you specify a collection of simple tag filters for resource types and tags, as supported by the AWS Tagging API GetResources operation. When more than one element is present, only resources that match all filters are part of the result. If a filter specifies more than one value for a key, a resource matches the filter if its tag value matches any of the specified values.
rqType :: Lens' ResourceQuery QueryType
rqType = lens _rqType (\ s a -> s{_rqType = a})

-- | The query that defines a group or a search.
rqSearchQuery :: Lens' ResourceQuery Text
rqSearchQuery = lens _rqSearchQuery (\ s a -> s{_rqSearchQuery = a})

instance FromJSON ResourceQuery where
        parseJSON
          = withObject "ResourceQuery"
              (\ x ->
                 ResourceQuery' <$> (x .: "Type") <*> (x .: "Query"))

instance Hashable ResourceQuery where

instance NFData ResourceQuery where

instance ToJSON ResourceQuery where
        toJSON ResourceQuery'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _rqType),
                  Just ("Query" .= _rqSearchQuery)])
