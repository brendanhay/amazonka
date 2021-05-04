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
-- Module      : Network.AWS.ResourceGroups.Types.ResourceQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceQuery where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroups.Types.QueryType

-- | The query that is used to define a resource group or a search for
-- resources. A query specifies both a query type and a query string as a
-- JSON object. See the examples section for example JSON strings.
--
-- The examples that follow are shown as standard JSON strings. If you
-- include such a string as a parameter to the AWS CLI or an SDK API, you
-- might need to \'escape\' the string into a single line. For example, see
-- the
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-quoting-strings.html Quoting strings>
-- in the /AWS CLI User Guide/.
--
-- __Example 1__
--
-- The following generic example shows a resource query JSON string that
-- includes only resources that meet the following criteria:
--
-- -   The resource type must be either @resource_type1@ or
--     @resource_type2@.
--
-- -   The resource must have a tag @Key1@ with a value of either @ValueA@
--     or @ValueB@.
--
-- -   The resource must have a tag @Key2@ with a value of either @ValueC@
--     or @ValueD@.
--
-- @{ \"Type\": \"TAG_FILTERS_1_0\", \"Query\": { \"ResourceTypeFilters\": [ \"resource_type1\", \"resource_type2\"], \"TagFilters\": [ { \"Key\": \"Key1\", \"Values\": [\"ValueA\",\"ValueB\"] }, { \"Key\":\"Key2\", \"Values\":[\"ValueC\",\"ValueD\"] } ] } }@
--
-- This has the equivalent \"shortcut\" syntax of the following:
--
-- @{ \"Type\": \"TAG_FILTERS_1_0\", \"Query\": { \"ResourceTypeFilters\": [ \"resource_type1\", \"resource_type2\"], \"TagFilters\": [ { \"Key1\": [\"ValueA\",\"ValueB\"] }, { \"Key2\": [\"ValueC\",\"ValueD\"] } ] } }@
--
-- __Example 2__
--
-- The following example shows a resource query JSON string that includes
-- only Amazon EC2 instances that are tagged @Stage@ with a value of
-- @Test@.
--
-- @{ \"Type\": \"TAG_FILTERS_1_0\", \"Query\": \"{ \"ResourceTypeFilters\": \"AWS::EC2::Instance\", \"TagFilters\": { \"Stage\": \"Test\" } } }@
--
-- __Example 3__
--
-- The following example shows a resource query JSON string that includes
-- resource of any supported type as long as it is tagged @Stage@ with a
-- value of @Prod@.
--
-- @{ \"Type\": \"TAG_FILTERS_1_0\", \"Query\": { \"ResourceTypeFilters\": \"AWS::AllSupported\", \"TagFilters\": { \"Stage\": \"Prod\" } } }@
--
-- __Example 4__
--
-- The following example shows a resource query JSON string that includes
-- only Amazon EC2 instances and Amazon S3 buckets that are part of the
-- specified AWS CloudFormation stack.
--
-- @{ \"Type\": \"CLOUDFORMATION_STACK_1_0\", \"Query\": { \"ResourceTypeFilters\": [ \"AWS::EC2::Instance\", \"AWS::S3::Bucket\" ], \"StackIdentifier\": \"arn:aws:cloudformation:us-west-2:123456789012:stack\/AWStestuseraccount\/fb0d5000-aba8-00e8-aa9e-50d5cEXAMPLE\" } }@
--
-- /See:/ 'newResourceQuery' smart constructor.
data ResourceQuery = ResourceQuery'
  { -- | The type of the query. You can use the following values:
    --
    -- -   /@CLOUDFORMATION_STACK_1_0:@/ Specifies that the @Query@ contains an
    --     ARN for a CloudFormation stack.
    --
    -- -   /@TAG_FILTERS_1_0:@/ Specifies that the @Query@ parameter contains a
    --     JSON string that represents a collection of simple tag filters for
    --     resource types and tags. The JSON string uses a syntax similar to
    --     the @ GetResources @ operation, but uses only the
    --     @  ResourceTypeFilters @ and @ TagFilters @ fields. If you specify
    --     more than one tag key, only resources that match all tag keys, and
    --     at least one value of each specified tag key, are returned in your
    --     query. If you specify more than one value for a tag key, a resource
    --     matches the filter if it has a tag key value that matches /any/ of
    --     the specified values.
    --
    --     For example, consider the following sample query for resources that
    --     have two tags, @Stage@ and @Version@, with two values each:
    --
    --     @[{\"Stage\":[\"Test\",\"Deploy\"]},{\"Version\":[\"1\",\"2\"]}]@
    --
    --     The results of this query could include the following.
    --
    --     -   An EC2 instance that has the following two tags:
    --         @{\"Stage\":\"Deploy\"}@, and @{\"Version\":\"2\"}@
    --
    --     -   An S3 bucket that has the following two tags:
    --         @{\"Stage\":\"Test\"}@, and @{\"Version\":\"1\"}@
    --
    --     The query would not include the following items in the results,
    --     however.
    --
    --     -   An EC2 instance that has only the following tag:
    --         @{\"Stage\":\"Deploy\"}@.
    --
    --         The instance does not have __all__ of the tag keys specified in
    --         the filter, so it is excluded from the results.
    --
    --     -   An RDS database that has the following two tags:
    --         @{\"Stage\":\"Archived\"}@ and @{\"Version\":\"4\"}@
    --
    --         The database has all of the tag keys, but none of those keys has
    --         an associated value that matches at least one of the specified
    --         values in the filter.
    type' :: QueryType,
    -- | The query that defines a group or a search.
    query :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'resourceQuery_type' - The type of the query. You can use the following values:
--
-- -   /@CLOUDFORMATION_STACK_1_0:@/ Specifies that the @Query@ contains an
--     ARN for a CloudFormation stack.
--
-- -   /@TAG_FILTERS_1_0:@/ Specifies that the @Query@ parameter contains a
--     JSON string that represents a collection of simple tag filters for
--     resource types and tags. The JSON string uses a syntax similar to
--     the @ GetResources @ operation, but uses only the
--     @  ResourceTypeFilters @ and @ TagFilters @ fields. If you specify
--     more than one tag key, only resources that match all tag keys, and
--     at least one value of each specified tag key, are returned in your
--     query. If you specify more than one value for a tag key, a resource
--     matches the filter if it has a tag key value that matches /any/ of
--     the specified values.
--
--     For example, consider the following sample query for resources that
--     have two tags, @Stage@ and @Version@, with two values each:
--
--     @[{\"Stage\":[\"Test\",\"Deploy\"]},{\"Version\":[\"1\",\"2\"]}]@
--
--     The results of this query could include the following.
--
--     -   An EC2 instance that has the following two tags:
--         @{\"Stage\":\"Deploy\"}@, and @{\"Version\":\"2\"}@
--
--     -   An S3 bucket that has the following two tags:
--         @{\"Stage\":\"Test\"}@, and @{\"Version\":\"1\"}@
--
--     The query would not include the following items in the results,
--     however.
--
--     -   An EC2 instance that has only the following tag:
--         @{\"Stage\":\"Deploy\"}@.
--
--         The instance does not have __all__ of the tag keys specified in
--         the filter, so it is excluded from the results.
--
--     -   An RDS database that has the following two tags:
--         @{\"Stage\":\"Archived\"}@ and @{\"Version\":\"4\"}@
--
--         The database has all of the tag keys, but none of those keys has
--         an associated value that matches at least one of the specified
--         values in the filter.
--
-- 'query', 'resourceQuery_searchQuery' - The query that defines a group or a search.
newResourceQuery ::
  -- | 'type''
  QueryType ->
  -- | 'query'
  Prelude.Text ->
  ResourceQuery
newResourceQuery pType_ pSearchQuery_ =
  ResourceQuery'
    { type' = pType_,
      query = pSearchQuery_
    }

-- | The type of the query. You can use the following values:
--
-- -   /@CLOUDFORMATION_STACK_1_0:@/ Specifies that the @Query@ contains an
--     ARN for a CloudFormation stack.
--
-- -   /@TAG_FILTERS_1_0:@/ Specifies that the @Query@ parameter contains a
--     JSON string that represents a collection of simple tag filters for
--     resource types and tags. The JSON string uses a syntax similar to
--     the @ GetResources @ operation, but uses only the
--     @  ResourceTypeFilters @ and @ TagFilters @ fields. If you specify
--     more than one tag key, only resources that match all tag keys, and
--     at least one value of each specified tag key, are returned in your
--     query. If you specify more than one value for a tag key, a resource
--     matches the filter if it has a tag key value that matches /any/ of
--     the specified values.
--
--     For example, consider the following sample query for resources that
--     have two tags, @Stage@ and @Version@, with two values each:
--
--     @[{\"Stage\":[\"Test\",\"Deploy\"]},{\"Version\":[\"1\",\"2\"]}]@
--
--     The results of this query could include the following.
--
--     -   An EC2 instance that has the following two tags:
--         @{\"Stage\":\"Deploy\"}@, and @{\"Version\":\"2\"}@
--
--     -   An S3 bucket that has the following two tags:
--         @{\"Stage\":\"Test\"}@, and @{\"Version\":\"1\"}@
--
--     The query would not include the following items in the results,
--     however.
--
--     -   An EC2 instance that has only the following tag:
--         @{\"Stage\":\"Deploy\"}@.
--
--         The instance does not have __all__ of the tag keys specified in
--         the filter, so it is excluded from the results.
--
--     -   An RDS database that has the following two tags:
--         @{\"Stage\":\"Archived\"}@ and @{\"Version\":\"4\"}@
--
--         The database has all of the tag keys, but none of those keys has
--         an associated value that matches at least one of the specified
--         values in the filter.
resourceQuery_type :: Lens.Lens' ResourceQuery QueryType
resourceQuery_type = Lens.lens (\ResourceQuery' {type'} -> type') (\s@ResourceQuery' {} a -> s {type' = a} :: ResourceQuery)

-- | The query that defines a group or a search.
resourceQuery_searchQuery :: Lens.Lens' ResourceQuery Prelude.Text
resourceQuery_searchQuery = Lens.lens (\ResourceQuery' {query} -> query) (\s@ResourceQuery' {} a -> s {query = a} :: ResourceQuery)

instance Prelude.FromJSON ResourceQuery where
  parseJSON =
    Prelude.withObject
      "ResourceQuery"
      ( \x ->
          ResourceQuery'
            Prelude.<$> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "Query")
      )

instance Prelude.Hashable ResourceQuery

instance Prelude.NFData ResourceQuery

instance Prelude.ToJSON ResourceQuery where
  toJSON ResourceQuery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just ("Query" Prelude..= query)
          ]
      )
