{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceGroupsTagging.GetResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the tagged or previously tagged resources that are located
-- in the specified Amazon Web Services Region for the account.
--
-- Depending on what information you want returned, you can also specify
-- the following:
--
-- -   /Filters/ that specify what tags and resource types you want
--     returned. The response includes all tags that are associated with
--     the requested resources.
--
-- -   Information about compliance with the account\'s effective tag
--     policy. For more information on tag policies, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies>
--     in the /Organizations User Guide./
--
-- This operation supports pagination, where the response can be sent in
-- multiple pages. You should check the @PaginationToken@ response
-- parameter to determine if there are additional results available to
-- return. Repeat the query, passing the @PaginationToken@ response
-- parameter value as an input to the next request until you recieve a
-- @null@ value. A null value for @PaginationToken@ indicates that there
-- are no more results waiting to be returned.
--
-- This operation returns paginated results.
module Amazonka.ResourceGroupsTagging.GetResources
  ( -- * Creating a Request
    GetResources (..),
    newGetResources,

    -- * Request Lenses
    getResources_paginationToken,
    getResources_tagsPerPage,
    getResources_resourceARNList,
    getResources_includeComplianceDetails,
    getResources_tagFilters,
    getResources_excludeCompliantResources,
    getResources_resourcesPerPage,
    getResources_resourceTypeFilters,

    -- * Destructuring the Response
    GetResourcesResponse (..),
    newGetResourcesResponse,

    -- * Response Lenses
    getResourcesResponse_paginationToken,
    getResourcesResponse_resourceTagMappingList,
    getResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroupsTagging.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResources' smart constructor.
data GetResources = GetResources'
  { -- | Specifies a @PaginationToken@ response value from a previous request to
    -- indicate that you want the next page of results. Leave this parameter
    -- empty in your initial request.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | Amazon Web Services recommends using @ResourcesPerPage@ instead of this
    -- parameter.
    --
    -- A limit that restricts the number of tags (key and value pairs) returned
    -- by @GetResources@ in paginated output. A resource with no tags is
    -- counted as having one tag (one key and value pair).
    --
    -- @GetResources@ does not split a resource and its associated tags across
    -- pages. If the specified @TagsPerPage@ would cause such a break, a
    -- @PaginationToken@ is returned in place of the affected resource and its
    -- tags. Use that token in another request to get the remaining data. For
    -- example, if you specify a @TagsPerPage@ of @100@ and the account has 22
    -- resources with 10 tags each (meaning that each resource has 10 key and
    -- value pairs), the output will consist of three pages. The first page
    -- displays the first 10 resources, each with its 10 tags. The second page
    -- displays the next 10 resources, each with its 10 tags. The third page
    -- displays the remaining 2 resources, each with its 10 tags.
    --
    -- You can set @TagsPerPage@ to a minimum of 100 items up to a maximum of
    -- 500 items.
    tagsPerPage :: Prelude.Maybe Prelude.Int,
    -- | Specifies a list of ARNs of resources for which you want to retrieve tag
    -- data. You can\'t specify both this parameter and any of the pagination
    -- parameters (@ResourcesPerPage@, @TagsPerPage@, @PaginationToken@) in the
    -- same request. If you specify both, you get an @Invalid Parameter@
    -- exception.
    --
    -- If a resource specified by this parameter doesn\'t exist, it doesn\'t
    -- generate an error; it simply isn\'t included in the response.
    --
    -- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    resourceARNList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies whether to include details regarding the compliance with the
    -- effective tag policy. Set this to @true@ to determine whether resources
    -- are compliant with the tag policy and to get details.
    includeComplianceDetails :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a list of TagFilters (keys and values) to restrict the output
    -- to only those resources that have tags with the specified keys and, if
    -- included, the specified values. Each @TagFilter@ must contain a key with
    -- values optional. A request can include up to 50 keys, and each key can
    -- include up to 20 values.
    --
    -- Note the following when deciding how to use TagFilters:
    --
    -- -   If you /don\'t/ specify a @TagFilter@, the response includes all
    --     resources that are currently tagged or ever had a tag. Resources
    --     that currently don\'t have tags are shown with an empty tag set,
    --     like this: @\"Tags\": []@.
    --
    -- -   If you specify more than one filter in a single request, the
    --     response returns only those resources that satisfy all filters.
    --
    -- -   If you specify a filter that contains more than one value for a key,
    --     the response returns resources that match /any/ of the specified
    --     values for that key.
    --
    -- -   If you don\'t specify a value for a key, the response returns all
    --     resources that are tagged with that key, with any or no value.
    --
    --     For example, for the following filters: @filter1= {keyA,{value1}}@,
    --     @filter2={keyB,{value2,value3,value4}}@, @filter3= {keyC}@:
    --
    --     -   @GetResources({filter1})@ returns resources tagged with
    --         @key1=value1@
    --
    --     -   @GetResources({filter2})@ returns resources tagged with
    --         @key2=value2@ or @key2=value3@ or @key2=value4@
    --
    --     -   @GetResources({filter3})@ returns resources tagged with any tag
    --         with the key @key3@, and with any or no value
    --
    --     -   @GetResources({filter1,filter2,filter3})@ returns resources
    --         tagged with
    --         @(key1=value1) and (key2=value2 or key2=value3 or key2=value4) and (key3, any or no value)@
    tagFilters :: Prelude.Maybe [TagFilter],
    -- | Specifies whether to exclude resources that are compliant with the tag
    -- policy. Set this to @true@ if you are interested in retrieving
    -- information on noncompliant resources only.
    --
    -- You can use this parameter only if the @IncludeComplianceDetails@
    -- parameter is also set to @true@.
    excludeCompliantResources :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the maximum number of results to be returned in each page. A
    -- query can return fewer than this maximum, even if there are more results
    -- still to return. You should always check the @PaginationToken@ response
    -- value to see if there are more results. You can specify a minimum of 1
    -- and a maximum value of 100.
    resourcesPerPage :: Prelude.Maybe Prelude.Int,
    -- | Specifies the resource types that you want included in the response. The
    -- format of each resource type is @service[:resourceType]@. For example,
    -- specifying a resource type of @ec2@ returns all Amazon EC2 resources
    -- (which includes EC2 instances). Specifying a resource type of
    -- @ec2:instance@ returns only EC2 instances.
    --
    -- The string for each service name and resource type is the same as that
    -- embedded in a resource\'s Amazon Resource Name (ARN). For the list of
    -- services whose resources you can use in this parameter, see
    -- <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
    --
    -- You can specify multiple resource types by using an array. The array can
    -- include up to 100 items. Note that the length constraint requirement
    -- applies to each resource type filter. For example, the following string
    -- would limit the response to only Amazon EC2 instances, Amazon S3
    -- buckets, or any Audit Manager resource:
    --
    -- @ec2:instance,s3:bucket,auditmanager@
    resourceTypeFilters :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'getResources_paginationToken' - Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
--
-- 'tagsPerPage', 'getResources_tagsPerPage' - Amazon Web Services recommends using @ResourcesPerPage@ instead of this
-- parameter.
--
-- A limit that restricts the number of tags (key and value pairs) returned
-- by @GetResources@ in paginated output. A resource with no tags is
-- counted as having one tag (one key and value pair).
--
-- @GetResources@ does not split a resource and its associated tags across
-- pages. If the specified @TagsPerPage@ would cause such a break, a
-- @PaginationToken@ is returned in place of the affected resource and its
-- tags. Use that token in another request to get the remaining data. For
-- example, if you specify a @TagsPerPage@ of @100@ and the account has 22
-- resources with 10 tags each (meaning that each resource has 10 key and
-- value pairs), the output will consist of three pages. The first page
-- displays the first 10 resources, each with its 10 tags. The second page
-- displays the next 10 resources, each with its 10 tags. The third page
-- displays the remaining 2 resources, each with its 10 tags.
--
-- You can set @TagsPerPage@ to a minimum of 100 items up to a maximum of
-- 500 items.
--
-- 'resourceARNList', 'getResources_resourceARNList' - Specifies a list of ARNs of resources for which you want to retrieve tag
-- data. You can\'t specify both this parameter and any of the pagination
-- parameters (@ResourcesPerPage@, @TagsPerPage@, @PaginationToken@) in the
-- same request. If you specify both, you get an @Invalid Parameter@
-- exception.
--
-- If a resource specified by this parameter doesn\'t exist, it doesn\'t
-- generate an error; it simply isn\'t included in the response.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
--
-- 'includeComplianceDetails', 'getResources_includeComplianceDetails' - Specifies whether to include details regarding the compliance with the
-- effective tag policy. Set this to @true@ to determine whether resources
-- are compliant with the tag policy and to get details.
--
-- 'tagFilters', 'getResources_tagFilters' - Specifies a list of TagFilters (keys and values) to restrict the output
-- to only those resources that have tags with the specified keys and, if
-- included, the specified values. Each @TagFilter@ must contain a key with
-- values optional. A request can include up to 50 keys, and each key can
-- include up to 20 values.
--
-- Note the following when deciding how to use TagFilters:
--
-- -   If you /don\'t/ specify a @TagFilter@, the response includes all
--     resources that are currently tagged or ever had a tag. Resources
--     that currently don\'t have tags are shown with an empty tag set,
--     like this: @\"Tags\": []@.
--
-- -   If you specify more than one filter in a single request, the
--     response returns only those resources that satisfy all filters.
--
-- -   If you specify a filter that contains more than one value for a key,
--     the response returns resources that match /any/ of the specified
--     values for that key.
--
-- -   If you don\'t specify a value for a key, the response returns all
--     resources that are tagged with that key, with any or no value.
--
--     For example, for the following filters: @filter1= {keyA,{value1}}@,
--     @filter2={keyB,{value2,value3,value4}}@, @filter3= {keyC}@:
--
--     -   @GetResources({filter1})@ returns resources tagged with
--         @key1=value1@
--
--     -   @GetResources({filter2})@ returns resources tagged with
--         @key2=value2@ or @key2=value3@ or @key2=value4@
--
--     -   @GetResources({filter3})@ returns resources tagged with any tag
--         with the key @key3@, and with any or no value
--
--     -   @GetResources({filter1,filter2,filter3})@ returns resources
--         tagged with
--         @(key1=value1) and (key2=value2 or key2=value3 or key2=value4) and (key3, any or no value)@
--
-- 'excludeCompliantResources', 'getResources_excludeCompliantResources' - Specifies whether to exclude resources that are compliant with the tag
-- policy. Set this to @true@ if you are interested in retrieving
-- information on noncompliant resources only.
--
-- You can use this parameter only if the @IncludeComplianceDetails@
-- parameter is also set to @true@.
--
-- 'resourcesPerPage', 'getResources_resourcesPerPage' - Specifies the maximum number of results to be returned in each page. A
-- query can return fewer than this maximum, even if there are more results
-- still to return. You should always check the @PaginationToken@ response
-- value to see if there are more results. You can specify a minimum of 1
-- and a maximum value of 100.
--
-- 'resourceTypeFilters', 'getResources_resourceTypeFilters' - Specifies the resource types that you want included in the response. The
-- format of each resource type is @service[:resourceType]@. For example,
-- specifying a resource type of @ec2@ returns all Amazon EC2 resources
-- (which includes EC2 instances). Specifying a resource type of
-- @ec2:instance@ returns only EC2 instances.
--
-- The string for each service name and resource type is the same as that
-- embedded in a resource\'s Amazon Resource Name (ARN). For the list of
-- services whose resources you can use in this parameter, see
-- <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
--
-- You can specify multiple resource types by using an array. The array can
-- include up to 100 items. Note that the length constraint requirement
-- applies to each resource type filter. For example, the following string
-- would limit the response to only Amazon EC2 instances, Amazon S3
-- buckets, or any Audit Manager resource:
--
-- @ec2:instance,s3:bucket,auditmanager@
newGetResources ::
  GetResources
newGetResources =
  GetResources'
    { paginationToken = Prelude.Nothing,
      tagsPerPage = Prelude.Nothing,
      resourceARNList = Prelude.Nothing,
      includeComplianceDetails = Prelude.Nothing,
      tagFilters = Prelude.Nothing,
      excludeCompliantResources = Prelude.Nothing,
      resourcesPerPage = Prelude.Nothing,
      resourceTypeFilters = Prelude.Nothing
    }

-- | Specifies a @PaginationToken@ response value from a previous request to
-- indicate that you want the next page of results. Leave this parameter
-- empty in your initial request.
getResources_paginationToken :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Text)
getResources_paginationToken = Lens.lens (\GetResources' {paginationToken} -> paginationToken) (\s@GetResources' {} a -> s {paginationToken = a} :: GetResources)

-- | Amazon Web Services recommends using @ResourcesPerPage@ instead of this
-- parameter.
--
-- A limit that restricts the number of tags (key and value pairs) returned
-- by @GetResources@ in paginated output. A resource with no tags is
-- counted as having one tag (one key and value pair).
--
-- @GetResources@ does not split a resource and its associated tags across
-- pages. If the specified @TagsPerPage@ would cause such a break, a
-- @PaginationToken@ is returned in place of the affected resource and its
-- tags. Use that token in another request to get the remaining data. For
-- example, if you specify a @TagsPerPage@ of @100@ and the account has 22
-- resources with 10 tags each (meaning that each resource has 10 key and
-- value pairs), the output will consist of three pages. The first page
-- displays the first 10 resources, each with its 10 tags. The second page
-- displays the next 10 resources, each with its 10 tags. The third page
-- displays the remaining 2 resources, each with its 10 tags.
--
-- You can set @TagsPerPage@ to a minimum of 100 items up to a maximum of
-- 500 items.
getResources_tagsPerPage :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Int)
getResources_tagsPerPage = Lens.lens (\GetResources' {tagsPerPage} -> tagsPerPage) (\s@GetResources' {} a -> s {tagsPerPage = a} :: GetResources)

-- | Specifies a list of ARNs of resources for which you want to retrieve tag
-- data. You can\'t specify both this parameter and any of the pagination
-- parameters (@ResourcesPerPage@, @TagsPerPage@, @PaginationToken@) in the
-- same request. If you specify both, you get an @Invalid Parameter@
-- exception.
--
-- If a resource specified by this parameter doesn\'t exist, it doesn\'t
-- generate an error; it simply isn\'t included in the response.
--
-- An ARN (Amazon Resource Name) uniquely identifies a resource. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
getResources_resourceARNList :: Lens.Lens' GetResources (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getResources_resourceARNList = Lens.lens (\GetResources' {resourceARNList} -> resourceARNList) (\s@GetResources' {} a -> s {resourceARNList = a} :: GetResources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to include details regarding the compliance with the
-- effective tag policy. Set this to @true@ to determine whether resources
-- are compliant with the tag policy and to get details.
getResources_includeComplianceDetails :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Bool)
getResources_includeComplianceDetails = Lens.lens (\GetResources' {includeComplianceDetails} -> includeComplianceDetails) (\s@GetResources' {} a -> s {includeComplianceDetails = a} :: GetResources)

-- | Specifies a list of TagFilters (keys and values) to restrict the output
-- to only those resources that have tags with the specified keys and, if
-- included, the specified values. Each @TagFilter@ must contain a key with
-- values optional. A request can include up to 50 keys, and each key can
-- include up to 20 values.
--
-- Note the following when deciding how to use TagFilters:
--
-- -   If you /don\'t/ specify a @TagFilter@, the response includes all
--     resources that are currently tagged or ever had a tag. Resources
--     that currently don\'t have tags are shown with an empty tag set,
--     like this: @\"Tags\": []@.
--
-- -   If you specify more than one filter in a single request, the
--     response returns only those resources that satisfy all filters.
--
-- -   If you specify a filter that contains more than one value for a key,
--     the response returns resources that match /any/ of the specified
--     values for that key.
--
-- -   If you don\'t specify a value for a key, the response returns all
--     resources that are tagged with that key, with any or no value.
--
--     For example, for the following filters: @filter1= {keyA,{value1}}@,
--     @filter2={keyB,{value2,value3,value4}}@, @filter3= {keyC}@:
--
--     -   @GetResources({filter1})@ returns resources tagged with
--         @key1=value1@
--
--     -   @GetResources({filter2})@ returns resources tagged with
--         @key2=value2@ or @key2=value3@ or @key2=value4@
--
--     -   @GetResources({filter3})@ returns resources tagged with any tag
--         with the key @key3@, and with any or no value
--
--     -   @GetResources({filter1,filter2,filter3})@ returns resources
--         tagged with
--         @(key1=value1) and (key2=value2 or key2=value3 or key2=value4) and (key3, any or no value)@
getResources_tagFilters :: Lens.Lens' GetResources (Prelude.Maybe [TagFilter])
getResources_tagFilters = Lens.lens (\GetResources' {tagFilters} -> tagFilters) (\s@GetResources' {} a -> s {tagFilters = a} :: GetResources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to exclude resources that are compliant with the tag
-- policy. Set this to @true@ if you are interested in retrieving
-- information on noncompliant resources only.
--
-- You can use this parameter only if the @IncludeComplianceDetails@
-- parameter is also set to @true@.
getResources_excludeCompliantResources :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Bool)
getResources_excludeCompliantResources = Lens.lens (\GetResources' {excludeCompliantResources} -> excludeCompliantResources) (\s@GetResources' {} a -> s {excludeCompliantResources = a} :: GetResources)

-- | Specifies the maximum number of results to be returned in each page. A
-- query can return fewer than this maximum, even if there are more results
-- still to return. You should always check the @PaginationToken@ response
-- value to see if there are more results. You can specify a minimum of 1
-- and a maximum value of 100.
getResources_resourcesPerPage :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Int)
getResources_resourcesPerPage = Lens.lens (\GetResources' {resourcesPerPage} -> resourcesPerPage) (\s@GetResources' {} a -> s {resourcesPerPage = a} :: GetResources)

-- | Specifies the resource types that you want included in the response. The
-- format of each resource type is @service[:resourceType]@. For example,
-- specifying a resource type of @ec2@ returns all Amazon EC2 resources
-- (which includes EC2 instances). Specifying a resource type of
-- @ec2:instance@ returns only EC2 instances.
--
-- The string for each service name and resource type is the same as that
-- embedded in a resource\'s Amazon Resource Name (ARN). For the list of
-- services whose resources you can use in this parameter, see
-- <https://docs.aws.amazon.com/resourcegroupstagging/latest/APIReference/supported-services.html Services that support the Resource Groups Tagging API>.
--
-- You can specify multiple resource types by using an array. The array can
-- include up to 100 items. Note that the length constraint requirement
-- applies to each resource type filter. For example, the following string
-- would limit the response to only Amazon EC2 instances, Amazon S3
-- buckets, or any Audit Manager resource:
--
-- @ec2:instance,s3:bucket,auditmanager@
getResources_resourceTypeFilters :: Lens.Lens' GetResources (Prelude.Maybe [Prelude.Text])
getResources_resourceTypeFilters = Lens.lens (\GetResources' {resourceTypeFilters} -> resourceTypeFilters) (\s@GetResources' {} a -> s {resourceTypeFilters = a} :: GetResources) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager GetResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourcesResponse_paginationToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourcesResponse_resourceTagMappingList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResources_paginationToken
          Lens..~ rs
          Lens.^? getResourcesResponse_paginationToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResources where
  type AWSResponse GetResources = GetResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Prelude.<$> (x Core..?> "PaginationToken")
            Prelude.<*> ( x Core..?> "ResourceTagMappingList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResources where
  hashWithSalt _salt GetResources' {..} =
    _salt `Prelude.hashWithSalt` paginationToken
      `Prelude.hashWithSalt` tagsPerPage
      `Prelude.hashWithSalt` resourceARNList
      `Prelude.hashWithSalt` includeComplianceDetails
      `Prelude.hashWithSalt` tagFilters
      `Prelude.hashWithSalt` excludeCompliantResources
      `Prelude.hashWithSalt` resourcesPerPage
      `Prelude.hashWithSalt` resourceTypeFilters

instance Prelude.NFData GetResources where
  rnf GetResources' {..} =
    Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf tagsPerPage
      `Prelude.seq` Prelude.rnf resourceARNList
      `Prelude.seq` Prelude.rnf includeComplianceDetails
      `Prelude.seq` Prelude.rnf tagFilters
      `Prelude.seq` Prelude.rnf excludeCompliantResources
      `Prelude.seq` Prelude.rnf resourcesPerPage
      `Prelude.seq` Prelude.rnf resourceTypeFilters

instance Core.ToHeaders GetResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.GetResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResources where
  toJSON GetResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PaginationToken" Core..=)
              Prelude.<$> paginationToken,
            ("TagsPerPage" Core..=) Prelude.<$> tagsPerPage,
            ("ResourceARNList" Core..=)
              Prelude.<$> resourceARNList,
            ("IncludeComplianceDetails" Core..=)
              Prelude.<$> includeComplianceDetails,
            ("TagFilters" Core..=) Prelude.<$> tagFilters,
            ("ExcludeCompliantResources" Core..=)
              Prelude.<$> excludeCompliantResources,
            ("ResourcesPerPage" Core..=)
              Prelude.<$> resourcesPerPage,
            ("ResourceTypeFilters" Core..=)
              Prelude.<$> resourceTypeFilters
          ]
      )

instance Core.ToPath GetResources where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { -- | A string that indicates that there is more data available than this
    -- response contains. To receive the next part of the response, specify
    -- this response value as the @PaginationToken@ value in the request for
    -- the next page.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | A list of resource ARNs and the tags (keys and values) associated with
    -- each.
    resourceTagMappingList :: Prelude.Maybe [ResourceTagMapping],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'getResourcesResponse_paginationToken' - A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
--
-- 'resourceTagMappingList', 'getResourcesResponse_resourceTagMappingList' - A list of resource ARNs and the tags (keys and values) associated with
-- each.
--
-- 'httpStatus', 'getResourcesResponse_httpStatus' - The response's http status code.
newGetResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcesResponse
newGetResourcesResponse pHttpStatus_ =
  GetResourcesResponse'
    { paginationToken =
        Prelude.Nothing,
      resourceTagMappingList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that there is more data available than this
-- response contains. To receive the next part of the response, specify
-- this response value as the @PaginationToken@ value in the request for
-- the next page.
getResourcesResponse_paginationToken :: Lens.Lens' GetResourcesResponse (Prelude.Maybe Prelude.Text)
getResourcesResponse_paginationToken = Lens.lens (\GetResourcesResponse' {paginationToken} -> paginationToken) (\s@GetResourcesResponse' {} a -> s {paginationToken = a} :: GetResourcesResponse)

-- | A list of resource ARNs and the tags (keys and values) associated with
-- each.
getResourcesResponse_resourceTagMappingList :: Lens.Lens' GetResourcesResponse (Prelude.Maybe [ResourceTagMapping])
getResourcesResponse_resourceTagMappingList = Lens.lens (\GetResourcesResponse' {resourceTagMappingList} -> resourceTagMappingList) (\s@GetResourcesResponse' {} a -> s {resourceTagMappingList = a} :: GetResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourcesResponse_httpStatus :: Lens.Lens' GetResourcesResponse Prelude.Int
getResourcesResponse_httpStatus = Lens.lens (\GetResourcesResponse' {httpStatus} -> httpStatus) (\s@GetResourcesResponse' {} a -> s {httpStatus = a} :: GetResourcesResponse)

instance Prelude.NFData GetResourcesResponse where
  rnf GetResourcesResponse' {..} =
    Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf resourceTagMappingList
      `Prelude.seq` Prelude.rnf httpStatus
