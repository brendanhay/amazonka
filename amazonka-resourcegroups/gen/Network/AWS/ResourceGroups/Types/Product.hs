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

-- | A filter name and value pair that is used to obtain more specific results from a list of groups.
--
--
--
-- /See:/ 'groupFilter' smart constructor.
data GroupFilter = GroupFilter'
  { _gfName   :: !GroupFilterName
  , _gfValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfName' - The name of the filter. Filter names are case-sensitive.
--
-- * 'gfValues' - One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
groupFilter
    :: GroupFilterName -- ^ 'gfName'
    -> NonEmpty Text -- ^ 'gfValues'
    -> GroupFilter
groupFilter pName_ pValues_ =
  GroupFilter' {_gfName = pName_, _gfValues = _List1 # pValues_}


-- | The name of the filter. Filter names are case-sensitive.
gfName :: Lens' GroupFilter GroupFilterName
gfName = lens _gfName (\ s a -> s{_gfName = a})

-- | One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
gfValues :: Lens' GroupFilter (NonEmpty Text)
gfValues = lens _gfValues (\ s a -> s{_gfValues = a}) . _List1

instance Hashable GroupFilter where

instance NFData GroupFilter where

instance ToJSON GroupFilter where
        toJSON GroupFilter'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _gfName),
                  Just ("Values" .= _gfValues)])

-- | The ARN and group name of a group.
--
--
--
-- /See:/ 'groupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { _giGroupARN  :: !(Maybe Text)
  , _giGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giGroupARN' - The ARN of a resource group.
--
-- * 'giGroupName' - The name of a resource group.
groupIdentifier
    :: GroupIdentifier
groupIdentifier =
  GroupIdentifier' {_giGroupARN = Nothing, _giGroupName = Nothing}


-- | The ARN of a resource group.
giGroupARN :: Lens' GroupIdentifier (Maybe Text)
giGroupARN = lens _giGroupARN (\ s a -> s{_giGroupARN = a})

-- | The name of a resource group.
giGroupName :: Lens' GroupIdentifier (Maybe Text)
giGroupName = lens _giGroupName (\ s a -> s{_giGroupName = a})

instance FromJSON GroupIdentifier where
        parseJSON
          = withObject "GroupIdentifier"
              (\ x ->
                 GroupIdentifier' <$>
                   (x .:? "GroupArn") <*> (x .:? "GroupName"))

instance Hashable GroupIdentifier where

instance NFData GroupIdentifier where

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

-- | A two-part error structure that can occur in @ListGroupResources@ or @SearchResources@ operations on CloudFormation stack-based queries. The error occurs if the CloudFormation stack on which the query is based either does not exist, or has a status that renders the stack inactive. A @QueryError@ occurrence does not necessarily mean that AWS Resource Groups could not complete the operation, but the resulting group might have no member resources.
--
--
--
-- /See:/ 'queryError' smart constructor.
data QueryError = QueryError'
  { _qeErrorCode :: !(Maybe QueryErrorCode)
  , _qeMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qeErrorCode' - Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
--
-- * 'qeMessage' - A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
queryError
    :: QueryError
queryError = QueryError' {_qeErrorCode = Nothing, _qeMessage = Nothing}


-- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and @CLOUDFORMATION_STACK_NOT_EXISTING@ .
qeErrorCode :: Lens' QueryError (Maybe QueryErrorCode)
qeErrorCode = lens _qeErrorCode (\ s a -> s{_qeErrorCode = a})

-- | A message that explains the @ErrorCode@ value. Messages might state that the specified CloudFormation stack does not exist (or no longer exists). For @CLOUDFORMATION_STACK_INACTIVE@ , the message typically states that the CloudFormation stack has a status that is not (or no longer) active, such as @CREATE_FAILED@ .
qeMessage :: Lens' QueryError (Maybe Text)
qeMessage = lens _qeMessage (\ s a -> s{_qeMessage = a})

instance FromJSON QueryError where
        parseJSON
          = withObject "QueryError"
              (\ x ->
                 QueryError' <$>
                   (x .:? "ErrorCode") <*> (x .:? "Message"))

instance Hashable QueryError where

instance NFData QueryError where

-- | A filter name and value pair that is used to obtain more specific results from a list of resources.
--
--
--
-- /See:/ 'resourceFilter' smart constructor.
data ResourceFilter = ResourceFilter'
  { _rfName   :: !ResourceFilterName
  , _rfValues :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfName' - The name of the filter. Filter names are case-sensitive.
--
-- * 'rfValues' - One or more filter values. Allowed filter values vary by resource filter name, and are case-sensitive.
resourceFilter
    :: ResourceFilterName -- ^ 'rfName'
    -> NonEmpty Text -- ^ 'rfValues'
    -> ResourceFilter
resourceFilter pName_ pValues_ =
  ResourceFilter' {_rfName = pName_, _rfValues = _List1 # pValues_}


-- | The name of the filter. Filter names are case-sensitive.
rfName :: Lens' ResourceFilter ResourceFilterName
rfName = lens _rfName (\ s a -> s{_rfName = a})

-- | One or more filter values. Allowed filter values vary by resource filter name, and are case-sensitive.
rfValues :: Lens' ResourceFilter (NonEmpty Text)
rfValues = lens _rfValues (\ s a -> s{_rfValues = a}) . _List1

instance Hashable ResourceFilter where

instance NFData ResourceFilter where

instance ToJSON ResourceFilter where
        toJSON ResourceFilter'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _rfName),
                  Just ("Values" .= _rfValues)])

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
-- * 'rqType' - The type of the query. The valid values in this release are @TAG_FILTERS_1_0@ and @CLOUDFORMATION_STACK_1_0@ . /@TAG_FILTERS_1_0:@ / A JSON syntax that lets you specify a collection of simple tag filters for resource types and tags, as supported by the AWS Tagging API <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> operation. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values. For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each. (@[{"Key":"Stage","Values":["Test","Deploy"]},{"Key":"Version","Values":["1","2"]}]@ ) The results of this query might include the following.     * An EC2 instance that has the following two tags: @{"Key":"Stage","Values":["Deploy"]}@ , and @{"Key":"Version","Values":["2"]}@      * An S3 bucket that has the following two tags: {"Key":"Stage","Values":["Test","Deploy"]}, and {"Key":"Version","Values":["1"]} The query would not return the following results, however. The following EC2 instance does not have all tag keys specified in the filter, so it is rejected. The RDS database has all of the tag keys, but no values that match at least one of the specified tag key values in the filter.     * An EC2 instance that has only the following tag: @{"Key":"Stage","Values":["Deploy"]}@ .     * An RDS database that has the following two tags: @{"Key":"Stage","Values":["Archived"]}@ , and @{"Key":"Version","Values":["4"]}@  /@CLOUDFORMATION_STACK_1_0:@ / A JSON syntax that lets you specify a CloudFormation stack ARN.
--
-- * 'rqSearchQuery' - The query that defines a group or a search.
resourceQuery
    :: QueryType -- ^ 'rqType'
    -> Text -- ^ 'rqSearchQuery'
    -> ResourceQuery
resourceQuery pType_ pSearchQuery_ =
  ResourceQuery' {_rqType = pType_, _rqSearchQuery = pSearchQuery_}


-- | The type of the query. The valid values in this release are @TAG_FILTERS_1_0@ and @CLOUDFORMATION_STACK_1_0@ . /@TAG_FILTERS_1_0:@ / A JSON syntax that lets you specify a collection of simple tag filters for resource types and tags, as supported by the AWS Tagging API <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> operation. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values. For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each. (@[{"Key":"Stage","Values":["Test","Deploy"]},{"Key":"Version","Values":["1","2"]}]@ ) The results of this query might include the following.     * An EC2 instance that has the following two tags: @{"Key":"Stage","Values":["Deploy"]}@ , and @{"Key":"Version","Values":["2"]}@      * An S3 bucket that has the following two tags: {"Key":"Stage","Values":["Test","Deploy"]}, and {"Key":"Version","Values":["1"]} The query would not return the following results, however. The following EC2 instance does not have all tag keys specified in the filter, so it is rejected. The RDS database has all of the tag keys, but no values that match at least one of the specified tag key values in the filter.     * An EC2 instance that has only the following tag: @{"Key":"Stage","Values":["Deploy"]}@ .     * An RDS database that has the following two tags: @{"Key":"Stage","Values":["Archived"]}@ , and @{"Key":"Version","Values":["4"]}@  /@CLOUDFORMATION_STACK_1_0:@ / A JSON syntax that lets you specify a CloudFormation stack ARN.
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
