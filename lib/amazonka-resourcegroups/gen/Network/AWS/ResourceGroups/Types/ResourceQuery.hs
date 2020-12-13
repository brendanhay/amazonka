{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceQuery
  ( ResourceQuery (..),

    -- * Smart constructor
    mkResourceQuery,

    -- * Lenses
    rqSearchQuery,
    rqType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ResourceGroups.Types.QueryType

-- | The query that is used to define a resource group or a search for resources. A query specifies both a query type and a query string as a JSON object. See the examples section for example JSON strings.
--
-- The examples that follow are shown as standard JSON strings. If you include such a string as a parameter to the AWS CLI or an SDK API, you might need to 'escape' the string into a single line. For example, see the <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-quoting-strings.html Quoting strings> in the /AWS CLI User Guide/ .
-- __Example 1__
-- The following generic example shows a resource query JSON string that includes only resources that meet the following criteria:
--
--     * The resource type must be either @resource_type1@ or @resource_type2@ .
--
--
--     * The resource must have a tag @Key1@ with a value of either @ValueA@ or @ValueB@ .
--
--
--     * The resource must have a tag @Key2@ with a value of either @ValueC@ or @ValueD@ .
--
--
-- @{ "Type": "TAG_FILTERS_1_0", "Query": { "ResourceTypeFilters": [ "resource_type1", "resource_type2"], "TagFilters": [ { "Key": "Key1", "Values": ["ValueA","ValueB"] }, { "Key":"Key2", "Values":["ValueC","ValueD"] } ] } }@
-- This has the equivalent "shortcut" syntax of the following:
-- @{ "Type": "TAG_FILTERS_1_0", "Query": { "ResourceTypeFilters": [ "resource_type1", "resource_type2"], "TagFilters": [ { "Key1": ["ValueA","ValueB"] }, { "Key2": ["ValueC","ValueD"] } ] } }@
-- __Example 2__
-- The following example shows a resource query JSON string that includes only Amazon EC2 instances that are tagged @Stage@ with a value of @Test@ .
-- @{ "Type": "TAG_FILTERS_1_0", "Query": "{ "ResourceTypeFilters": "AWS::EC2::Instance", "TagFilters": { "Stage": "Test" } } }@
-- __Example 3__
-- The following example shows a resource query JSON string that includes resource of any supported type as long as it is tagged @Stage@ with a value of @Prod@ .
-- @{ "Type": "TAG_FILTERS_1_0", "Query": { "ResourceTypeFilters": "AWS::AllSupported", "TagFilters": { "Stage": "Prod" } } }@
-- __Example 4__
-- The following example shows a resource query JSON string that includes only Amazon EC2 instances and Amazon S3 buckets that are part of the specified AWS CloudFormation stack.
-- @{ "Type": "CLOUDFORMATION_STACK_1_0", "Query": { "ResourceTypeFilters": [ "AWS::EC2::Instance", "AWS::S3::Bucket" ], "StackIdentifier": "arn:aws:cloudformation:us-west-2:123456789012:stack/AWStestuseraccount/fb0d5000-aba8-00e8-aa9e-50d5cEXAMPLE" } }@
--
-- /See:/ 'mkResourceQuery' smart constructor.
data ResourceQuery = ResourceQuery'
  { -- | The query that defines a group or a search.
    searchQuery :: Lude.Text,
    -- | The type of the query. You can use the following values:
    --
    --
    --     * /@CLOUDFORMATION_STACK_1_0:@ / Specifies that the @Query@ contains an ARN for a CloudFormation stack.
    --
    --
    --     * /@TAG_FILTERS_1_0:@ / Specifies that the @Query@ parameter contains a JSON string that represents a collection of simple tag filters for resource types and tags. The JSON string uses a syntax similar to the @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> @ operation, but uses only the @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-ResourceTypeFilters ResourceTypeFilters> @ and @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-TagFiltersTagFilters TagFilters> @ fields. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values.
    -- For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each:
    -- @[{"Stage":["Test","Deploy"]},{"Version":["1","2"]}]@
    -- The results of this query could include the following.
    --
    --     * An EC2 instance that has the following two tags: @{"Stage":"Deploy"}@ , and @{"Version":"2"}@
    --
    --
    --     * An S3 bucket that has the following two tags: @{"Stage":"Test"}@ , and @{"Version":"1"}@
    --
    --
    -- The query would not include the following items in the results, however.
    --
    --     * An EC2 instance that has only the following tag: @{"Stage":"Deploy"}@ .
    -- The instance does not have __all__ of the tag keys specified in the filter, so it is excluded from the results.
    --
    --
    --     * An RDS database that has the following two tags: @{"Stage":"Archived"}@ and @{"Version":"4"}@
    -- The database has all of the tag keys, but none of those keys has an associated value that matches at least one of the specified values in the filter.
    type' :: QueryType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceQuery' with the minimum fields required to make a request.
--
-- * 'searchQuery' - The query that defines a group or a search.
-- * 'type'' - The type of the query. You can use the following values:
--
--
--     * /@CLOUDFORMATION_STACK_1_0:@ / Specifies that the @Query@ contains an ARN for a CloudFormation stack.
--
--
--     * /@TAG_FILTERS_1_0:@ / Specifies that the @Query@ parameter contains a JSON string that represents a collection of simple tag filters for resource types and tags. The JSON string uses a syntax similar to the @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> @ operation, but uses only the @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-ResourceTypeFilters ResourceTypeFilters> @ and @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-TagFiltersTagFilters TagFilters> @ fields. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values.
-- For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each:
-- @[{"Stage":["Test","Deploy"]},{"Version":["1","2"]}]@
-- The results of this query could include the following.
--
--     * An EC2 instance that has the following two tags: @{"Stage":"Deploy"}@ , and @{"Version":"2"}@
--
--
--     * An S3 bucket that has the following two tags: @{"Stage":"Test"}@ , and @{"Version":"1"}@
--
--
-- The query would not include the following items in the results, however.
--
--     * An EC2 instance that has only the following tag: @{"Stage":"Deploy"}@ .
-- The instance does not have __all__ of the tag keys specified in the filter, so it is excluded from the results.
--
--
--     * An RDS database that has the following two tags: @{"Stage":"Archived"}@ and @{"Version":"4"}@
-- The database has all of the tag keys, but none of those keys has an associated value that matches at least one of the specified values in the filter.
mkResourceQuery ::
  -- | 'searchQuery'
  Lude.Text ->
  -- | 'type''
  QueryType ->
  ResourceQuery
mkResourceQuery pSearchQuery_ pType_ =
  ResourceQuery' {searchQuery = pSearchQuery_, type' = pType_}

-- | The query that defines a group or a search.
--
-- /Note:/ Consider using 'searchQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqSearchQuery :: Lens.Lens' ResourceQuery Lude.Text
rqSearchQuery = Lens.lens (searchQuery :: ResourceQuery -> Lude.Text) (\s a -> s {searchQuery = a} :: ResourceQuery)
{-# DEPRECATED rqSearchQuery "Use generic-lens or generic-optics with 'searchQuery' instead." #-}

-- | The type of the query. You can use the following values:
--
--
--     * /@CLOUDFORMATION_STACK_1_0:@ / Specifies that the @Query@ contains an ARN for a CloudFormation stack.
--
--
--     * /@TAG_FILTERS_1_0:@ / Specifies that the @Query@ parameter contains a JSON string that represents a collection of simple tag filters for resource types and tags. The JSON string uses a syntax similar to the @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources> @ operation, but uses only the @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-ResourceTypeFilters ResourceTypeFilters> @ and @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-TagFiltersTagFilters TagFilters> @ fields. If you specify more than one tag key, only resources that match all tag keys, and at least one value of each specified tag key, are returned in your query. If you specify more than one value for a tag key, a resource matches the filter if it has a tag key value that matches /any/ of the specified values.
-- For example, consider the following sample query for resources that have two tags, @Stage@ and @Version@ , with two values each:
-- @[{"Stage":["Test","Deploy"]},{"Version":["1","2"]}]@
-- The results of this query could include the following.
--
--     * An EC2 instance that has the following two tags: @{"Stage":"Deploy"}@ , and @{"Version":"2"}@
--
--
--     * An S3 bucket that has the following two tags: @{"Stage":"Test"}@ , and @{"Version":"1"}@
--
--
-- The query would not include the following items in the results, however.
--
--     * An EC2 instance that has only the following tag: @{"Stage":"Deploy"}@ .
-- The instance does not have __all__ of the tag keys specified in the filter, so it is excluded from the results.
--
--
--     * An RDS database that has the following two tags: @{"Stage":"Archived"}@ and @{"Version":"4"}@
-- The database has all of the tag keys, but none of those keys has an associated value that matches at least one of the specified values in the filter.
--
--
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqType :: Lens.Lens' ResourceQuery QueryType
rqType = Lens.lens (type' :: ResourceQuery -> QueryType) (\s a -> s {type' = a} :: ResourceQuery)
{-# DEPRECATED rqType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ResourceQuery where
  parseJSON =
    Lude.withObject
      "ResourceQuery"
      ( \x ->
          ResourceQuery'
            Lude.<$> (x Lude..: "Query") Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON ResourceQuery where
  toJSON ResourceQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Query" Lude..= searchQuery),
            Lude.Just ("Type" Lude..= type')
          ]
      )
