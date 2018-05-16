{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLaunchTemplates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch templates.
--
--
module Network.AWS.EC2.DescribeLaunchTemplates
    (
    -- * Creating a Request
      describeLaunchTemplates
    , DescribeLaunchTemplates
    -- * Request Lenses
    , dltsFilters
    , dltsNextToken
    , dltsLaunchTemplateIds
    , dltsDryRun
    , dltsMaxResults
    , dltsLaunchTemplateNames

    -- * Destructuring the Response
    , describeLaunchTemplatesResponse
    , DescribeLaunchTemplatesResponse
    -- * Response Lenses
    , dltsrsLaunchTemplates
    , dltsrsNextToken
    , dltsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLaunchTemplates' smart constructor.
data DescribeLaunchTemplates = DescribeLaunchTemplates'
  { _dltsFilters             :: !(Maybe [Filter])
  , _dltsNextToken           :: !(Maybe Text)
  , _dltsLaunchTemplateIds   :: !(Maybe [Text])
  , _dltsDryRun              :: !(Maybe Bool)
  , _dltsMaxResults          :: !(Maybe Int)
  , _dltsLaunchTemplateNames :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLaunchTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltsFilters' - One or more filters.     * @create-time@ - The time the launch template was created.     * @launch-template-name@ - The name of the launch template.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of the tag's key). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.
--
-- * 'dltsNextToken' - The token to request the next page of results.
--
-- * 'dltsLaunchTemplateIds' - One or more launch template IDs.
--
-- * 'dltsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dltsMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
--
-- * 'dltsLaunchTemplateNames' - One or more launch template names.
describeLaunchTemplates
    :: DescribeLaunchTemplates
describeLaunchTemplates =
  DescribeLaunchTemplates'
    { _dltsFilters = Nothing
    , _dltsNextToken = Nothing
    , _dltsLaunchTemplateIds = Nothing
    , _dltsDryRun = Nothing
    , _dltsMaxResults = Nothing
    , _dltsLaunchTemplateNames = Nothing
    }


-- | One or more filters.     * @create-time@ - The time the launch template was created.     * @launch-template-name@ - The name of the launch template.     * @tag@ :/key/ =/value/ - The key/value combination of a tag assigned to the resource. Specify the key of the tag in the filter name and the value of the tag in the filter value. For example, for the tag Purpose=X, specify @tag:Purpose@ for the filter name and @X@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. This filter is independent of the @tag-value@ filter. For example, if you use both the filter "tag-key=Purpose" and the filter "tag-value=X", you get any resources assigned both the tag key Purpose (regardless of what the tag's value is), and the tag value X (regardless of the tag's key). If you want to list only resources where Purpose is X, see the @tag@ :/key/ =/value/ filter.
dltsFilters :: Lens' DescribeLaunchTemplates [Filter]
dltsFilters = lens _dltsFilters (\ s a -> s{_dltsFilters = a}) . _Default . _Coerce

-- | The token to request the next page of results.
dltsNextToken :: Lens' DescribeLaunchTemplates (Maybe Text)
dltsNextToken = lens _dltsNextToken (\ s a -> s{_dltsNextToken = a})

-- | One or more launch template IDs.
dltsLaunchTemplateIds :: Lens' DescribeLaunchTemplates [Text]
dltsLaunchTemplateIds = lens _dltsLaunchTemplateIds (\ s a -> s{_dltsLaunchTemplateIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltsDryRun :: Lens' DescribeLaunchTemplates (Maybe Bool)
dltsDryRun = lens _dltsDryRun (\ s a -> s{_dltsDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
dltsMaxResults :: Lens' DescribeLaunchTemplates (Maybe Int)
dltsMaxResults = lens _dltsMaxResults (\ s a -> s{_dltsMaxResults = a})

-- | One or more launch template names.
dltsLaunchTemplateNames :: Lens' DescribeLaunchTemplates [Text]
dltsLaunchTemplateNames = lens _dltsLaunchTemplateNames (\ s a -> s{_dltsLaunchTemplateNames = a}) . _Default . _Coerce

instance AWSRequest DescribeLaunchTemplates where
        type Rs DescribeLaunchTemplates =
             DescribeLaunchTemplatesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeLaunchTemplatesResponse' <$>
                   (x .@? "launchTemplates" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLaunchTemplates where

instance NFData DescribeLaunchTemplates where

instance ToHeaders DescribeLaunchTemplates where
        toHeaders = const mempty

instance ToPath DescribeLaunchTemplates where
        toPath = const "/"

instance ToQuery DescribeLaunchTemplates where
        toQuery DescribeLaunchTemplates'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLaunchTemplates" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dltsFilters),
               "NextToken" =: _dltsNextToken,
               toQuery
                 (toQueryList "LaunchTemplateId" <$>
                    _dltsLaunchTemplateIds),
               "DryRun" =: _dltsDryRun,
               "MaxResults" =: _dltsMaxResults,
               toQuery
                 (toQueryList "LaunchTemplateName" <$>
                    _dltsLaunchTemplateNames)]

-- | /See:/ 'describeLaunchTemplatesResponse' smart constructor.
data DescribeLaunchTemplatesResponse = DescribeLaunchTemplatesResponse'
  { _dltsrsLaunchTemplates :: !(Maybe [LaunchTemplate])
  , _dltsrsNextToken       :: !(Maybe Text)
  , _dltsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLaunchTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltsrsLaunchTemplates' - Information about the launch templates.
--
-- * 'dltsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dltsrsResponseStatus' - -- | The response status code.
describeLaunchTemplatesResponse
    :: Int -- ^ 'dltsrsResponseStatus'
    -> DescribeLaunchTemplatesResponse
describeLaunchTemplatesResponse pResponseStatus_ =
  DescribeLaunchTemplatesResponse'
    { _dltsrsLaunchTemplates = Nothing
    , _dltsrsNextToken = Nothing
    , _dltsrsResponseStatus = pResponseStatus_
    }


-- | Information about the launch templates.
dltsrsLaunchTemplates :: Lens' DescribeLaunchTemplatesResponse [LaunchTemplate]
dltsrsLaunchTemplates = lens _dltsrsLaunchTemplates (\ s a -> s{_dltsrsLaunchTemplates = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dltsrsNextToken :: Lens' DescribeLaunchTemplatesResponse (Maybe Text)
dltsrsNextToken = lens _dltsrsNextToken (\ s a -> s{_dltsrsNextToken = a})

-- | -- | The response status code.
dltsrsResponseStatus :: Lens' DescribeLaunchTemplatesResponse Int
dltsrsResponseStatus = lens _dltsrsResponseStatus (\ s a -> s{_dltsrsResponseStatus = a})

instance NFData DescribeLaunchTemplatesResponse where
