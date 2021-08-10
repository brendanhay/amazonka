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
-- Module      : Network.AWS.EC2.DescribeLaunchTemplateVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more versions of a specified launch template. You can
-- describe all versions, individual versions, or a range of versions. You
-- can also describe all the latest versions or all the default versions of
-- all the launch templates in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLaunchTemplateVersions
  ( -- * Creating a Request
    DescribeLaunchTemplateVersions (..),
    newDescribeLaunchTemplateVersions,

    -- * Request Lenses
    describeLaunchTemplateVersions_nextToken,
    describeLaunchTemplateVersions_versions,
    describeLaunchTemplateVersions_dryRun,
    describeLaunchTemplateVersions_maxResults,
    describeLaunchTemplateVersions_minVersion,
    describeLaunchTemplateVersions_launchTemplateId,
    describeLaunchTemplateVersions_launchTemplateName,
    describeLaunchTemplateVersions_maxVersion,
    describeLaunchTemplateVersions_filters,

    -- * Destructuring the Response
    DescribeLaunchTemplateVersionsResponse (..),
    newDescribeLaunchTemplateVersionsResponse,

    -- * Response Lenses
    describeLaunchTemplateVersionsResponse_nextToken,
    describeLaunchTemplateVersionsResponse_launchTemplateVersions,
    describeLaunchTemplateVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLaunchTemplateVersions' smart constructor.
data DescribeLaunchTemplateVersions = DescribeLaunchTemplateVersions'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more versions of the launch template. Valid values depend on
    -- whether you are describing a specified launch template (by ID or name)
    -- or all launch templates in your account.
    --
    -- To describe one or more versions of a specified launch template, valid
    -- values are @$Latest@, @$Default@, and numbers.
    --
    -- To describe all launch templates in your account that are defined as the
    -- latest version, the valid value is @$Latest@. To describe all launch
    -- templates in your account that are defined as the default version, the
    -- valid value is @$Default@. You can specify @$Latest@ and @$Default@ in
    -- the same call. You cannot specify numbers.
    versions :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 1 and 200.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The version number after which to describe launch template versions.
    minVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template. To describe one or more versions of a
    -- specified launch template, you must specify either the launch template
    -- ID or the launch template name in the request. To describe all the
    -- latest or default launch template versions in your account, you must
    -- omit this parameter.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template. To describe one or more versions of a
    -- specified launch template, you must specify either the launch template
    -- ID or the launch template name in the request. To describe all the
    -- latest or default launch template versions in your account, you must
    -- omit this parameter.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version number up to which to describe launch template versions.
    maxVersion :: Prelude.Maybe Prelude.Text,
    -- | One or more filters.
    --
    -- -   @create-time@ - The time the launch template version was created.
    --
    -- -   @ebs-optimized@ - A boolean that indicates whether the instance is
    --     optimized for Amazon EBS I\/O.
    --
    -- -   @iam-instance-profile@ - The ARN of the IAM instance profile.
    --
    -- -   @image-id@ - The ID of the AMI.
    --
    -- -   @instance-type@ - The instance type.
    --
    -- -   @is-default-version@ - A boolean that indicates whether the launch
    --     template version is the default version.
    --
    -- -   @kernel-id@ - The kernel ID.
    --
    -- -   @ram-disk-id@ - The RAM disk ID.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLaunchTemplateVersions_nextToken' - The token to request the next page of results.
--
-- 'versions', 'describeLaunchTemplateVersions_versions' - One or more versions of the launch template. Valid values depend on
-- whether you are describing a specified launch template (by ID or name)
-- or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid
-- values are @$Latest@, @$Default@, and numbers.
--
-- To describe all launch templates in your account that are defined as the
-- latest version, the valid value is @$Latest@. To describe all launch
-- templates in your account that are defined as the default version, the
-- valid value is @$Default@. You can specify @$Latest@ and @$Default@ in
-- the same call. You cannot specify numbers.
--
-- 'dryRun', 'describeLaunchTemplateVersions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLaunchTemplateVersions_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200.
--
-- 'minVersion', 'describeLaunchTemplateVersions_minVersion' - The version number after which to describe launch template versions.
--
-- 'launchTemplateId', 'describeLaunchTemplateVersions_launchTemplateId' - The ID of the launch template. To describe one or more versions of a
-- specified launch template, you must specify either the launch template
-- ID or the launch template name in the request. To describe all the
-- latest or default launch template versions in your account, you must
-- omit this parameter.
--
-- 'launchTemplateName', 'describeLaunchTemplateVersions_launchTemplateName' - The name of the launch template. To describe one or more versions of a
-- specified launch template, you must specify either the launch template
-- ID or the launch template name in the request. To describe all the
-- latest or default launch template versions in your account, you must
-- omit this parameter.
--
-- 'maxVersion', 'describeLaunchTemplateVersions_maxVersion' - The version number up to which to describe launch template versions.
--
-- 'filters', 'describeLaunchTemplateVersions_filters' - One or more filters.
--
-- -   @create-time@ - The time the launch template version was created.
--
-- -   @ebs-optimized@ - A boolean that indicates whether the instance is
--     optimized for Amazon EBS I\/O.
--
-- -   @iam-instance-profile@ - The ARN of the IAM instance profile.
--
-- -   @image-id@ - The ID of the AMI.
--
-- -   @instance-type@ - The instance type.
--
-- -   @is-default-version@ - A boolean that indicates whether the launch
--     template version is the default version.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @ram-disk-id@ - The RAM disk ID.
newDescribeLaunchTemplateVersions ::
  DescribeLaunchTemplateVersions
newDescribeLaunchTemplateVersions =
  DescribeLaunchTemplateVersions'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      minVersion = Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      maxVersion = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to request the next page of results.
describeLaunchTemplateVersions_nextToken :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
describeLaunchTemplateVersions_nextToken = Lens.lens (\DescribeLaunchTemplateVersions' {nextToken} -> nextToken) (\s@DescribeLaunchTemplateVersions' {} a -> s {nextToken = a} :: DescribeLaunchTemplateVersions)

-- | One or more versions of the launch template. Valid values depend on
-- whether you are describing a specified launch template (by ID or name)
-- or all launch templates in your account.
--
-- To describe one or more versions of a specified launch template, valid
-- values are @$Latest@, @$Default@, and numbers.
--
-- To describe all launch templates in your account that are defined as the
-- latest version, the valid value is @$Latest@. To describe all launch
-- templates in your account that are defined as the default version, the
-- valid value is @$Default@. You can specify @$Latest@ and @$Default@ in
-- the same call. You cannot specify numbers.
describeLaunchTemplateVersions_versions :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe [Prelude.Text])
describeLaunchTemplateVersions_versions = Lens.lens (\DescribeLaunchTemplateVersions' {versions} -> versions) (\s@DescribeLaunchTemplateVersions' {} a -> s {versions = a} :: DescribeLaunchTemplateVersions) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLaunchTemplateVersions_dryRun :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Bool)
describeLaunchTemplateVersions_dryRun = Lens.lens (\DescribeLaunchTemplateVersions' {dryRun} -> dryRun) (\s@DescribeLaunchTemplateVersions' {} a -> s {dryRun = a} :: DescribeLaunchTemplateVersions)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200.
describeLaunchTemplateVersions_maxResults :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Int)
describeLaunchTemplateVersions_maxResults = Lens.lens (\DescribeLaunchTemplateVersions' {maxResults} -> maxResults) (\s@DescribeLaunchTemplateVersions' {} a -> s {maxResults = a} :: DescribeLaunchTemplateVersions)

-- | The version number after which to describe launch template versions.
describeLaunchTemplateVersions_minVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
describeLaunchTemplateVersions_minVersion = Lens.lens (\DescribeLaunchTemplateVersions' {minVersion} -> minVersion) (\s@DescribeLaunchTemplateVersions' {} a -> s {minVersion = a} :: DescribeLaunchTemplateVersions)

-- | The ID of the launch template. To describe one or more versions of a
-- specified launch template, you must specify either the launch template
-- ID or the launch template name in the request. To describe all the
-- latest or default launch template versions in your account, you must
-- omit this parameter.
describeLaunchTemplateVersions_launchTemplateId :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
describeLaunchTemplateVersions_launchTemplateId = Lens.lens (\DescribeLaunchTemplateVersions' {launchTemplateId} -> launchTemplateId) (\s@DescribeLaunchTemplateVersions' {} a -> s {launchTemplateId = a} :: DescribeLaunchTemplateVersions)

-- | The name of the launch template. To describe one or more versions of a
-- specified launch template, you must specify either the launch template
-- ID or the launch template name in the request. To describe all the
-- latest or default launch template versions in your account, you must
-- omit this parameter.
describeLaunchTemplateVersions_launchTemplateName :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
describeLaunchTemplateVersions_launchTemplateName = Lens.lens (\DescribeLaunchTemplateVersions' {launchTemplateName} -> launchTemplateName) (\s@DescribeLaunchTemplateVersions' {} a -> s {launchTemplateName = a} :: DescribeLaunchTemplateVersions)

-- | The version number up to which to describe launch template versions.
describeLaunchTemplateVersions_maxVersion :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
describeLaunchTemplateVersions_maxVersion = Lens.lens (\DescribeLaunchTemplateVersions' {maxVersion} -> maxVersion) (\s@DescribeLaunchTemplateVersions' {} a -> s {maxVersion = a} :: DescribeLaunchTemplateVersions)

-- | One or more filters.
--
-- -   @create-time@ - The time the launch template version was created.
--
-- -   @ebs-optimized@ - A boolean that indicates whether the instance is
--     optimized for Amazon EBS I\/O.
--
-- -   @iam-instance-profile@ - The ARN of the IAM instance profile.
--
-- -   @image-id@ - The ID of the AMI.
--
-- -   @instance-type@ - The instance type.
--
-- -   @is-default-version@ - A boolean that indicates whether the launch
--     template version is the default version.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @ram-disk-id@ - The RAM disk ID.
describeLaunchTemplateVersions_filters :: Lens.Lens' DescribeLaunchTemplateVersions (Prelude.Maybe [Filter])
describeLaunchTemplateVersions_filters = Lens.lens (\DescribeLaunchTemplateVersions' {filters} -> filters) (\s@DescribeLaunchTemplateVersions' {} a -> s {filters = a} :: DescribeLaunchTemplateVersions) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeLaunchTemplateVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLaunchTemplateVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLaunchTemplateVersionsResponse_launchTemplateVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLaunchTemplateVersions_nextToken
          Lens..~ rs
          Lens.^? describeLaunchTemplateVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeLaunchTemplateVersions
  where
  type
    AWSResponse DescribeLaunchTemplateVersions =
      DescribeLaunchTemplateVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLaunchTemplateVersionsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "launchTemplateVersionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLaunchTemplateVersions

instance
  Prelude.NFData
    DescribeLaunchTemplateVersions

instance
  Core.ToHeaders
    DescribeLaunchTemplateVersions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLaunchTemplateVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLaunchTemplateVersions where
  toQuery DescribeLaunchTemplateVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeLaunchTemplateVersions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "LaunchTemplateVersion"
              Prelude.<$> versions
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "MinVersion" Core.=: minVersion,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "LaunchTemplateName" Core.=: launchTemplateName,
        "MaxVersion" Core.=: maxVersion,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeLaunchTemplateVersionsResponse' smart constructor.
data DescribeLaunchTemplateVersionsResponse = DescribeLaunchTemplateVersionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the launch template versions.
    launchTemplateVersions :: Prelude.Maybe [LaunchTemplateVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLaunchTemplateVersionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'launchTemplateVersions', 'describeLaunchTemplateVersionsResponse_launchTemplateVersions' - Information about the launch template versions.
--
-- 'httpStatus', 'describeLaunchTemplateVersionsResponse_httpStatus' - The response's http status code.
newDescribeLaunchTemplateVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLaunchTemplateVersionsResponse
newDescribeLaunchTemplateVersionsResponse
  pHttpStatus_ =
    DescribeLaunchTemplateVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        launchTemplateVersions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLaunchTemplateVersionsResponse_nextToken :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Prelude.Maybe Prelude.Text)
describeLaunchTemplateVersionsResponse_nextToken = Lens.lens (\DescribeLaunchTemplateVersionsResponse' {nextToken} -> nextToken) (\s@DescribeLaunchTemplateVersionsResponse' {} a -> s {nextToken = a} :: DescribeLaunchTemplateVersionsResponse)

-- | Information about the launch template versions.
describeLaunchTemplateVersionsResponse_launchTemplateVersions :: Lens.Lens' DescribeLaunchTemplateVersionsResponse (Prelude.Maybe [LaunchTemplateVersion])
describeLaunchTemplateVersionsResponse_launchTemplateVersions = Lens.lens (\DescribeLaunchTemplateVersionsResponse' {launchTemplateVersions} -> launchTemplateVersions) (\s@DescribeLaunchTemplateVersionsResponse' {} a -> s {launchTemplateVersions = a} :: DescribeLaunchTemplateVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLaunchTemplateVersionsResponse_httpStatus :: Lens.Lens' DescribeLaunchTemplateVersionsResponse Prelude.Int
describeLaunchTemplateVersionsResponse_httpStatus = Lens.lens (\DescribeLaunchTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeLaunchTemplateVersionsResponse' {} a -> s {httpStatus = a} :: DescribeLaunchTemplateVersionsResponse)

instance
  Prelude.NFData
    DescribeLaunchTemplateVersionsResponse
