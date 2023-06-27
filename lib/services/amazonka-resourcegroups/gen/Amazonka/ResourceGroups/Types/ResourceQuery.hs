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
-- Module      : Amazonka.ResourceGroups.Types.ResourceQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.ResourceQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroups.Types.QueryType

-- | The query you can use to define a resource group or a search for
-- resources. A @ResourceQuery@ specifies both a query @Type@ and a @Query@
-- string as JSON string objects. See the examples section for example JSON
-- strings. For more information about creating a resource group with a
-- resource query, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/gettingstarted-query.html Build queries and groups in Resource Groups>
-- in the /Resource Groups User Guide/
--
-- When you combine all of the elements together into a single string, any
-- double quotes that are embedded inside another double quote pair must be
-- escaped by preceding the embedded double quote with a backslash
-- character (\\). For example, a complete @ResourceQuery@ parameter must
-- be formatted like the following CLI parameter example:
--
-- @--resource-query \'{\"Type\":\"TAG_FILTERS_1_0\",\"Query\":\"{\\\"ResourceTypeFilters\\\":[\\\"AWS::AllSupported\\\"],\\\"TagFilters\\\":[{\\\"Key\\\":\\\"Stage\\\",\\\"Values\\\":[\\\"Test\\\"]}]}\"}\'@
--
-- In the preceding example, all of the double quote characters in the
-- value part of the @Query@ element must be escaped because the value
-- itself is surrounded by double quotes. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-quoting-strings.html Quoting strings>
-- in the /Command Line Interface User Guide/.
--
-- For the complete list of resource types that you can use in the array
-- value for @ResourceTypeFilters@, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/supported-resources.html Resources you can use with Resource Groups and Tag Editor>
-- in the /Resource Groups User Guide/. For example:
--
-- @\"ResourceTypeFilters\":[\"AWS::S3::Bucket\", \"AWS::EC2::Instance\"]@
--
-- /See:/ 'newResourceQuery' smart constructor.
data ResourceQuery = ResourceQuery'
  { -- | The type of the query to perform. This can have one of two values:
    --
    -- -   /@CLOUDFORMATION_STACK_1_0:@/ Specifies that you want the group to
    --     contain the members of an CloudFormation stack. The @Query@ contains
    --     a @StackIdentifier@ element with an ARN for a CloudFormation stack.
    --
    -- -   /@TAG_FILTERS_1_0:@/ Specifies that you want the group to include
    --     resource that have tags that match the query.
    type' :: QueryType,
    -- | The query that defines a group or a search. The contents depends on the
    -- value of the @Type@ element.
    --
    -- -   @ResourceTypeFilters@ – Applies to all @ResourceQuery@ objects of
    --     either @Type@. This element contains one of the following two items:
    --
    --     -   The value @AWS::AllSupported@. This causes the ResourceQuery to
    --         match resources of any resource type that also match the query.
    --
    --     -   A list (a JSON array) of resource type identifiers that limit
    --         the query to only resources of the specified types. For the
    --         complete list of resource types that you can use in the array
    --         value for @ResourceTypeFilters@, see
    --         <https://docs.aws.amazon.com/ARG/latest/userguide/supported-resources.html Resources you can use with Resource Groups and Tag Editor>
    --         in the /Resource Groups User Guide/.
    --
    --     Example: @\"ResourceTypeFilters\": [\"AWS::AllSupported\"]@ or
    --     @\"ResourceTypeFilters\": [\"AWS::EC2::Instance\", \"AWS::S3::Bucket\"]@
    --
    -- -   @TagFilters@ – applicable only if @Type@ = @TAG_FILTERS_1_0@. The
    --     @Query@ contains a JSON string that represents a collection of
    --     simple tag filters. The JSON string uses a syntax similar to the
    --     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources>@ @
    --     operation, but uses only the
    --     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-ResourceTypeFilters ResourceTypeFilters>@ @
    --     and
    --     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-TagFiltersTagFilters TagFilters>@ @
    --     fields. If you specify more than one tag key, only resources that
    --     match all tag keys, and at least one value of each specified tag
    --     key, are returned in your query. If you specify more than one value
    --     for a tag key, a resource matches the filter if it has a tag key
    --     value that matches /any/ of the specified values.
    --
    --     For example, consider the following sample query for resources that
    --     have two tags, @Stage@ and @Version@, with two values each:
    --
    --     @[{\"Stage\":[\"Test\",\"Deploy\"]},{\"Version\":[\"1\",\"2\"]}]@
    --
    --     The results of this resource query could include the following.
    --
    --     -   An Amazon EC2 instance that has the following two tags:
    --         @{\"Stage\":\"Deploy\"}@, and @{\"Version\":\"2\"}@
    --
    --     -   An S3 bucket that has the following two tags:
    --         @{\"Stage\":\"Test\"}@, and @{\"Version\":\"1\"}@
    --
    --     The resource query results would /not/ include the following items
    --     in the results, however.
    --
    --     -   An Amazon EC2 instance that has only the following tag:
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
    --     Example:
    --     @\"TagFilters\": [ { \"Key\": \"Stage\", \"Values\": [ \"Gamma\", \"Beta\" ] }@
    --
    -- -   @StackIdentifier@ – applicable only if @Type@ =
    --     @CLOUDFORMATION_STACK_1_0@. The value of this parameter is the
    --     Amazon Resource Name (ARN) of the CloudFormation stack whose
    --     resources you want included in the group.
    query :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'resourceQuery_type' - The type of the query to perform. This can have one of two values:
--
-- -   /@CLOUDFORMATION_STACK_1_0:@/ Specifies that you want the group to
--     contain the members of an CloudFormation stack. The @Query@ contains
--     a @StackIdentifier@ element with an ARN for a CloudFormation stack.
--
-- -   /@TAG_FILTERS_1_0:@/ Specifies that you want the group to include
--     resource that have tags that match the query.
--
-- 'query', 'resourceQuery_searchQuery' - The query that defines a group or a search. The contents depends on the
-- value of the @Type@ element.
--
-- -   @ResourceTypeFilters@ – Applies to all @ResourceQuery@ objects of
--     either @Type@. This element contains one of the following two items:
--
--     -   The value @AWS::AllSupported@. This causes the ResourceQuery to
--         match resources of any resource type that also match the query.
--
--     -   A list (a JSON array) of resource type identifiers that limit
--         the query to only resources of the specified types. For the
--         complete list of resource types that you can use in the array
--         value for @ResourceTypeFilters@, see
--         <https://docs.aws.amazon.com/ARG/latest/userguide/supported-resources.html Resources you can use with Resource Groups and Tag Editor>
--         in the /Resource Groups User Guide/.
--
--     Example: @\"ResourceTypeFilters\": [\"AWS::AllSupported\"]@ or
--     @\"ResourceTypeFilters\": [\"AWS::EC2::Instance\", \"AWS::S3::Bucket\"]@
--
-- -   @TagFilters@ – applicable only if @Type@ = @TAG_FILTERS_1_0@. The
--     @Query@ contains a JSON string that represents a collection of
--     simple tag filters. The JSON string uses a syntax similar to the
--     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources>@ @
--     operation, but uses only the
--     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-ResourceTypeFilters ResourceTypeFilters>@ @
--     and
--     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-TagFiltersTagFilters TagFilters>@ @
--     fields. If you specify more than one tag key, only resources that
--     match all tag keys, and at least one value of each specified tag
--     key, are returned in your query. If you specify more than one value
--     for a tag key, a resource matches the filter if it has a tag key
--     value that matches /any/ of the specified values.
--
--     For example, consider the following sample query for resources that
--     have two tags, @Stage@ and @Version@, with two values each:
--
--     @[{\"Stage\":[\"Test\",\"Deploy\"]},{\"Version\":[\"1\",\"2\"]}]@
--
--     The results of this resource query could include the following.
--
--     -   An Amazon EC2 instance that has the following two tags:
--         @{\"Stage\":\"Deploy\"}@, and @{\"Version\":\"2\"}@
--
--     -   An S3 bucket that has the following two tags:
--         @{\"Stage\":\"Test\"}@, and @{\"Version\":\"1\"}@
--
--     The resource query results would /not/ include the following items
--     in the results, however.
--
--     -   An Amazon EC2 instance that has only the following tag:
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
--     Example:
--     @\"TagFilters\": [ { \"Key\": \"Stage\", \"Values\": [ \"Gamma\", \"Beta\" ] }@
--
-- -   @StackIdentifier@ – applicable only if @Type@ =
--     @CLOUDFORMATION_STACK_1_0@. The value of this parameter is the
--     Amazon Resource Name (ARN) of the CloudFormation stack whose
--     resources you want included in the group.
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

-- | The type of the query to perform. This can have one of two values:
--
-- -   /@CLOUDFORMATION_STACK_1_0:@/ Specifies that you want the group to
--     contain the members of an CloudFormation stack. The @Query@ contains
--     a @StackIdentifier@ element with an ARN for a CloudFormation stack.
--
-- -   /@TAG_FILTERS_1_0:@/ Specifies that you want the group to include
--     resource that have tags that match the query.
resourceQuery_type :: Lens.Lens' ResourceQuery QueryType
resourceQuery_type = Lens.lens (\ResourceQuery' {type'} -> type') (\s@ResourceQuery' {} a -> s {type' = a} :: ResourceQuery)

-- | The query that defines a group or a search. The contents depends on the
-- value of the @Type@ element.
--
-- -   @ResourceTypeFilters@ – Applies to all @ResourceQuery@ objects of
--     either @Type@. This element contains one of the following two items:
--
--     -   The value @AWS::AllSupported@. This causes the ResourceQuery to
--         match resources of any resource type that also match the query.
--
--     -   A list (a JSON array) of resource type identifiers that limit
--         the query to only resources of the specified types. For the
--         complete list of resource types that you can use in the array
--         value for @ResourceTypeFilters@, see
--         <https://docs.aws.amazon.com/ARG/latest/userguide/supported-resources.html Resources you can use with Resource Groups and Tag Editor>
--         in the /Resource Groups User Guide/.
--
--     Example: @\"ResourceTypeFilters\": [\"AWS::AllSupported\"]@ or
--     @\"ResourceTypeFilters\": [\"AWS::EC2::Instance\", \"AWS::S3::Bucket\"]@
--
-- -   @TagFilters@ – applicable only if @Type@ = @TAG_FILTERS_1_0@. The
--     @Query@ contains a JSON string that represents a collection of
--     simple tag filters. The JSON string uses a syntax similar to the
--     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html GetResources>@ @
--     operation, but uses only the
--     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-ResourceTypeFilters ResourceTypeFilters>@ @
--     and
--     @ @<https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/API_GetResources.html#resourcegrouptagging-GetResources-request-TagFiltersTagFilters TagFilters>@ @
--     fields. If you specify more than one tag key, only resources that
--     match all tag keys, and at least one value of each specified tag
--     key, are returned in your query. If you specify more than one value
--     for a tag key, a resource matches the filter if it has a tag key
--     value that matches /any/ of the specified values.
--
--     For example, consider the following sample query for resources that
--     have two tags, @Stage@ and @Version@, with two values each:
--
--     @[{\"Stage\":[\"Test\",\"Deploy\"]},{\"Version\":[\"1\",\"2\"]}]@
--
--     The results of this resource query could include the following.
--
--     -   An Amazon EC2 instance that has the following two tags:
--         @{\"Stage\":\"Deploy\"}@, and @{\"Version\":\"2\"}@
--
--     -   An S3 bucket that has the following two tags:
--         @{\"Stage\":\"Test\"}@, and @{\"Version\":\"1\"}@
--
--     The resource query results would /not/ include the following items
--     in the results, however.
--
--     -   An Amazon EC2 instance that has only the following tag:
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
--     Example:
--     @\"TagFilters\": [ { \"Key\": \"Stage\", \"Values\": [ \"Gamma\", \"Beta\" ] }@
--
-- -   @StackIdentifier@ – applicable only if @Type@ =
--     @CLOUDFORMATION_STACK_1_0@. The value of this parameter is the
--     Amazon Resource Name (ARN) of the CloudFormation stack whose
--     resources you want included in the group.
resourceQuery_searchQuery :: Lens.Lens' ResourceQuery Prelude.Text
resourceQuery_searchQuery = Lens.lens (\ResourceQuery' {query} -> query) (\s@ResourceQuery' {} a -> s {query = a} :: ResourceQuery)

instance Data.FromJSON ResourceQuery where
  parseJSON =
    Data.withObject
      "ResourceQuery"
      ( \x ->
          ResourceQuery'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Query")
      )

instance Prelude.Hashable ResourceQuery where
  hashWithSalt _salt ResourceQuery' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` query

instance Prelude.NFData ResourceQuery where
  rnf ResourceQuery' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf query

instance Data.ToJSON ResourceQuery where
  toJSON ResourceQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Query" Data..= query)
          ]
      )
