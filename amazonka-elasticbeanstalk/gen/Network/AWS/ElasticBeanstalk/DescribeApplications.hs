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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of existing applications.
--
--
module Network.AWS.ElasticBeanstalk.DescribeApplications
    (
    -- * Creating a Request
      describeApplications
    , DescribeApplications
    -- * Request Lenses
    , daApplicationNames

    -- * Destructuring the Response
    , describeApplicationsResponse
    , DescribeApplicationsResponse
    -- * Response Lenses
    , darsApplications
    , darsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to describe one or more applications.
--
--
--
-- /See:/ 'describeApplications' smart constructor.
newtype DescribeApplications = DescribeApplications'
  { _daApplicationNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daApplicationNames' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
describeApplications
    :: DescribeApplications
describeApplications = DescribeApplications' {_daApplicationNames = Nothing}


-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to only include those with the specified names.
daApplicationNames :: Lens' DescribeApplications [Text]
daApplicationNames = lens _daApplicationNames (\ s a -> s{_daApplicationNames = a}) . _Default . _Coerce

instance AWSRequest DescribeApplications where
        type Rs DescribeApplications =
             DescribeApplicationsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeApplicationsResult"
              (\ s h x ->
                 DescribeApplicationsResponse' <$>
                   (x .@? "Applications" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeApplications where

instance NFData DescribeApplications where

instance ToHeaders DescribeApplications where
        toHeaders = const mempty

instance ToPath DescribeApplications where
        toPath = const "/"

instance ToQuery DescribeApplications where
        toQuery DescribeApplications'{..}
          = mconcat
              ["Action" =: ("DescribeApplications" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ApplicationNames" =:
                 toQuery
                   (toQueryList "member" <$> _daApplicationNames)]

-- | Result message containing a list of application descriptions.
--
--
--
-- /See:/ 'describeApplicationsResponse' smart constructor.
data DescribeApplicationsResponse = DescribeApplicationsResponse'
  { _darsApplications   :: !(Maybe [ApplicationDescription])
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsApplications' - This parameter contains a list of 'ApplicationDescription' .
--
-- * 'darsResponseStatus' - -- | The response status code.
describeApplicationsResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeApplicationsResponse
describeApplicationsResponse pResponseStatus_ =
  DescribeApplicationsResponse'
    {_darsApplications = Nothing, _darsResponseStatus = pResponseStatus_}


-- | This parameter contains a list of 'ApplicationDescription' .
darsApplications :: Lens' DescribeApplicationsResponse [ApplicationDescription]
darsApplications = lens _darsApplications (\ s a -> s{_darsApplications = a}) . _Default . _Coerce

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeApplicationsResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeApplicationsResponse where
