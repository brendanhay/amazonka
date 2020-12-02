{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLaunchTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more versions of a specified launch template. You can describe all versions, individual versions, or a range of versions. You can also describe all the latest versions or all the default versions of all the launch templates in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLaunchTemplateVersions
  ( -- * Creating a Request
    describeLaunchTemplateVersions,
    DescribeLaunchTemplateVersions,

    -- * Request Lenses
    dltvsLaunchTemplateName,
    dltvsLaunchTemplateId,
    dltvsMinVersion,
    dltvsFilters,
    dltvsMaxVersion,
    dltvsVersions,
    dltvsNextToken,
    dltvsDryRun,
    dltvsMaxResults,

    -- * Destructuring the Response
    describeLaunchTemplateVersionsResponse,
    DescribeLaunchTemplateVersionsResponse,

    -- * Response Lenses
    dltvrsNextToken,
    dltvrsLaunchTemplateVersions,
    dltvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLaunchTemplateVersions' smart constructor.
data DescribeLaunchTemplateVersions = DescribeLaunchTemplateVersions'
  { _dltvsLaunchTemplateName ::
      !(Maybe Text),
    _dltvsLaunchTemplateId ::
      !(Maybe Text),
    _dltvsMinVersion ::
      !(Maybe Text),
    _dltvsFilters ::
      !(Maybe [Filter]),
    _dltvsMaxVersion ::
      !(Maybe Text),
    _dltvsVersions ::
      !(Maybe [Text]),
    _dltvsNextToken ::
      !(Maybe Text),
    _dltvsDryRun :: !(Maybe Bool),
    _dltvsMaxResults ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLaunchTemplateVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvsLaunchTemplateName' - The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- * 'dltvsLaunchTemplateId' - The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
--
-- * 'dltvsMinVersion' - The version number after which to describe launch template versions.
--
-- * 'dltvsFilters' - One or more filters.     * @create-time@ - The time the launch template version was created.     * @ebs-optimized@ - A boolean that indicates whether the instance is optimized for Amazon EBS I/O.     * @iam-instance-profile@ - The ARN of the IAM instance profile.     * @image-id@ - The ID of the AMI.     * @instance-type@ - The instance type.     * @is-default-version@ - A boolean that indicates whether the launch template version is the default version.     * @kernel-id@ - The kernel ID.     * @ram-disk-id@ - The RAM disk ID.
--
-- * 'dltvsMaxVersion' - The version number up to which to describe launch template versions.
--
-- * 'dltvsVersions' - One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account. To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers. To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
--
-- * 'dltvsNextToken' - The token to request the next page of results.
--
-- * 'dltvsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dltvsMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
describeLaunchTemplateVersions ::
  DescribeLaunchTemplateVersions
describeLaunchTemplateVersions =
  DescribeLaunchTemplateVersions'
    { _dltvsLaunchTemplateName =
        Nothing,
      _dltvsLaunchTemplateId = Nothing,
      _dltvsMinVersion = Nothing,
      _dltvsFilters = Nothing,
      _dltvsMaxVersion = Nothing,
      _dltvsVersions = Nothing,
      _dltvsNextToken = Nothing,
      _dltvsDryRun = Nothing,
      _dltvsMaxResults = Nothing
    }

-- | The name of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
dltvsLaunchTemplateName :: Lens' DescribeLaunchTemplateVersions (Maybe Text)
dltvsLaunchTemplateName = lens _dltvsLaunchTemplateName (\s a -> s {_dltvsLaunchTemplateName = a})

-- | The ID of the launch template. To describe one or more versions of a specified launch template, you must specify either the launch template ID or the launch template name in the request. To describe all the latest or default launch template versions in your account, you must omit this parameter.
dltvsLaunchTemplateId :: Lens' DescribeLaunchTemplateVersions (Maybe Text)
dltvsLaunchTemplateId = lens _dltvsLaunchTemplateId (\s a -> s {_dltvsLaunchTemplateId = a})

-- | The version number after which to describe launch template versions.
dltvsMinVersion :: Lens' DescribeLaunchTemplateVersions (Maybe Text)
dltvsMinVersion = lens _dltvsMinVersion (\s a -> s {_dltvsMinVersion = a})

-- | One or more filters.     * @create-time@ - The time the launch template version was created.     * @ebs-optimized@ - A boolean that indicates whether the instance is optimized for Amazon EBS I/O.     * @iam-instance-profile@ - The ARN of the IAM instance profile.     * @image-id@ - The ID of the AMI.     * @instance-type@ - The instance type.     * @is-default-version@ - A boolean that indicates whether the launch template version is the default version.     * @kernel-id@ - The kernel ID.     * @ram-disk-id@ - The RAM disk ID.
dltvsFilters :: Lens' DescribeLaunchTemplateVersions [Filter]
dltvsFilters = lens _dltvsFilters (\s a -> s {_dltvsFilters = a}) . _Default . _Coerce

-- | The version number up to which to describe launch template versions.
dltvsMaxVersion :: Lens' DescribeLaunchTemplateVersions (Maybe Text)
dltvsMaxVersion = lens _dltvsMaxVersion (\s a -> s {_dltvsMaxVersion = a})

-- | One or more versions of the launch template. Valid values depend on whether you are describing a specified launch template (by ID or name) or all launch templates in your account. To describe one or more versions of a specified launch template, valid values are @> Latest@ , @> Default@ , and numbers. To describe all launch templates in your account that are defined as the latest version, the valid value is @> Latest@ . To describe all launch templates in your account that are defined as the default version, the valid value is @> Default@ . You can specify @> Latest@ and @> Default@ in the same call. You cannot specify numbers.
dltvsVersions :: Lens' DescribeLaunchTemplateVersions [Text]
dltvsVersions = lens _dltvsVersions (\s a -> s {_dltvsVersions = a}) . _Default . _Coerce

-- | The token to request the next page of results.
dltvsNextToken :: Lens' DescribeLaunchTemplateVersions (Maybe Text)
dltvsNextToken = lens _dltvsNextToken (\s a -> s {_dltvsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltvsDryRun :: Lens' DescribeLaunchTemplateVersions (Maybe Bool)
dltvsDryRun = lens _dltvsDryRun (\s a -> s {_dltvsDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 1 and 200.
dltvsMaxResults :: Lens' DescribeLaunchTemplateVersions (Maybe Int)
dltvsMaxResults = lens _dltvsMaxResults (\s a -> s {_dltvsMaxResults = a})

instance AWSPager DescribeLaunchTemplateVersions where
  page rq rs
    | stop (rs ^. dltvrsNextToken) = Nothing
    | stop (rs ^. dltvrsLaunchTemplateVersions) = Nothing
    | otherwise = Just $ rq & dltvsNextToken .~ rs ^. dltvrsNextToken

instance AWSRequest DescribeLaunchTemplateVersions where
  type
    Rs DescribeLaunchTemplateVersions =
      DescribeLaunchTemplateVersionsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeLaunchTemplateVersionsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "launchTemplateVersionSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeLaunchTemplateVersions

instance NFData DescribeLaunchTemplateVersions

instance ToHeaders DescribeLaunchTemplateVersions where
  toHeaders = const mempty

instance ToPath DescribeLaunchTemplateVersions where
  toPath = const "/"

instance ToQuery DescribeLaunchTemplateVersions where
  toQuery DescribeLaunchTemplateVersions' {..} =
    mconcat
      [ "Action" =: ("DescribeLaunchTemplateVersions" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "LaunchTemplateName" =: _dltvsLaunchTemplateName,
        "LaunchTemplateId" =: _dltvsLaunchTemplateId,
        "MinVersion" =: _dltvsMinVersion,
        toQuery (toQueryList "Filter" <$> _dltvsFilters),
        "MaxVersion" =: _dltvsMaxVersion,
        toQuery (toQueryList "LaunchTemplateVersion" <$> _dltvsVersions),
        "NextToken" =: _dltvsNextToken,
        "DryRun" =: _dltvsDryRun,
        "MaxResults" =: _dltvsMaxResults
      ]

-- | /See:/ 'describeLaunchTemplateVersionsResponse' smart constructor.
data DescribeLaunchTemplateVersionsResponse = DescribeLaunchTemplateVersionsResponse'
  { _dltvrsNextToken ::
      !(Maybe Text),
    _dltvrsLaunchTemplateVersions ::
      !( Maybe
           [LaunchTemplateVersion]
       ),
    _dltvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLaunchTemplateVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dltvrsLaunchTemplateVersions' - Information about the launch template versions.
--
-- * 'dltvrsResponseStatus' - -- | The response status code.
describeLaunchTemplateVersionsResponse ::
  -- | 'dltvrsResponseStatus'
  Int ->
  DescribeLaunchTemplateVersionsResponse
describeLaunchTemplateVersionsResponse pResponseStatus_ =
  DescribeLaunchTemplateVersionsResponse'
    { _dltvrsNextToken =
        Nothing,
      _dltvrsLaunchTemplateVersions = Nothing,
      _dltvrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dltvrsNextToken :: Lens' DescribeLaunchTemplateVersionsResponse (Maybe Text)
dltvrsNextToken = lens _dltvrsNextToken (\s a -> s {_dltvrsNextToken = a})

-- | Information about the launch template versions.
dltvrsLaunchTemplateVersions :: Lens' DescribeLaunchTemplateVersionsResponse [LaunchTemplateVersion]
dltvrsLaunchTemplateVersions = lens _dltvrsLaunchTemplateVersions (\s a -> s {_dltvrsLaunchTemplateVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
dltvrsResponseStatus :: Lens' DescribeLaunchTemplateVersionsResponse Int
dltvrsResponseStatus = lens _dltvrsResponseStatus (\s a -> s {_dltvrsResponseStatus = a})

instance NFData DescribeLaunchTemplateVersionsResponse
