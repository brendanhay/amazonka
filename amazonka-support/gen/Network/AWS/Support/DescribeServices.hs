{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeServices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the current list of AWS services and a list of service
-- categories that applies to each one. You then use service names and
-- categories in your CreateCase requests. Each AWS service has its own set
-- of categories.
--
-- The service codes and category codes correspond to the values that are
-- displayed in the __Service__ and __Category__ drop-down lists on the AWS
-- Support Center
-- <https://console.aws.amazon.com/support/home#/case/create Create Case>
-- page. The values in those fields, however, do not necessarily match the
-- service codes and categories returned by the @DescribeServices@ request.
-- Always use the service codes and categories obtained programmatically.
-- This practice ensures that you always have the most recent set of
-- service and category codes.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeServices.html>
module Network.AWS.Support.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , describeServices
    -- ** Request lenses
    , dsServiceCodeList
    , dsLanguage

    -- * Response
    , DescribeServicesResponse
    -- ** Response constructor
    , describeServicesResponse
    -- ** Response lenses
    , dsrsServices
    , dsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeServices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsServiceCodeList'
--
-- * 'dsLanguage'
data DescribeServices = DescribeServices'
    { _dsServiceCodeList :: !(Maybe [Text])
    , _dsLanguage        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServices' smart constructor.
describeServices :: DescribeServices
describeServices =
    DescribeServices'
    { _dsServiceCodeList = Nothing
    , _dsLanguage = Nothing
    }

-- | A JSON-formatted list of service codes available for AWS services.
dsServiceCodeList :: Lens' DescribeServices [Text]
dsServiceCodeList = lens _dsServiceCodeList (\ s a -> s{_dsServiceCodeList = a}) . _Default;

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dsLanguage :: Lens' DescribeServices (Maybe Text)
dsLanguage = lens _dsLanguage (\ s a -> s{_dsLanguage = a});

instance AWSRequest DescribeServices where
        type Sv DescribeServices = Support
        type Rs DescribeServices = DescribeServicesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServicesResponse' <$>
                   (x .?> "services" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeServices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeServices" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeServices where
        toJSON DescribeServices'{..}
          = object
              ["serviceCodeList" .= _dsServiceCodeList,
               "language" .= _dsLanguage]

instance ToPath DescribeServices where
        toPath = const "/"

instance ToQuery DescribeServices where
        toQuery = const mempty

-- | The list of AWS services returned by the DescribeServices operation.
--
-- /See:/ 'describeServicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrsServices'
--
-- * 'dsrsStatus'
data DescribeServicesResponse = DescribeServicesResponse'
    { _dsrsServices :: !(Maybe [SupportService])
    , _dsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServicesResponse' smart constructor.
describeServicesResponse :: Int -> DescribeServicesResponse
describeServicesResponse pStatus_ =
    DescribeServicesResponse'
    { _dsrsServices = Nothing
    , _dsrsStatus = pStatus_
    }

-- | A JSON-formatted list of AWS services.
dsrsServices :: Lens' DescribeServicesResponse [SupportService]
dsrsServices = lens _dsrsServices (\ s a -> s{_dsrsServices = a}) . _Default;

-- | FIXME: Undocumented member.
dsrsStatus :: Lens' DescribeServicesResponse Int
dsrsStatus = lens _dsrsStatus (\ s a -> s{_dsrsStatus = a});
