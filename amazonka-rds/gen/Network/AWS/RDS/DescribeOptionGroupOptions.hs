{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes all available options.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOptionGroupOptions.html>
module Network.AWS.RDS.DescribeOptionGroupOptions
    (
    -- * Request
      DescribeOptionGroupOptions
    -- ** Request constructor
    , describeOptionGroupOptions
    -- ** Request lenses
    , dogorqFilters
    , dogorqMajorEngineVersion
    , dogorqMaxRecords
    , dogorqMarker
    , dogorqEngineName

    -- * Response
    , DescribeOptionGroupOptionsResponse
    -- ** Response constructor
    , describeOptionGroupOptionsResponse
    -- ** Response lenses
    , dogorsOptionGroupOptions
    , dogorsMarker
    , dogorsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeOptionGroupOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogorqFilters'
--
-- * 'dogorqMajorEngineVersion'
--
-- * 'dogorqMaxRecords'
--
-- * 'dogorqMarker'
--
-- * 'dogorqEngineName'
data DescribeOptionGroupOptions = DescribeOptionGroupOptions'
    { _dogorqFilters            :: !(Maybe [Filter])
    , _dogorqMajorEngineVersion :: !(Maybe Text)
    , _dogorqMaxRecords         :: !(Maybe Int)
    , _dogorqMarker             :: !(Maybe Text)
    , _dogorqEngineName         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOptionGroupOptions' smart constructor.
describeOptionGroupOptions :: Text -> DescribeOptionGroupOptions
describeOptionGroupOptions pEngineName =
    DescribeOptionGroupOptions'
    { _dogorqFilters = Nothing
    , _dogorqMajorEngineVersion = Nothing
    , _dogorqMaxRecords = Nothing
    , _dogorqMarker = Nothing
    , _dogorqEngineName = pEngineName
    }

-- | This parameter is not currently supported.
dogorqFilters :: Lens' DescribeOptionGroupOptions [Filter]
dogorqFilters = lens _dogorqFilters (\ s a -> s{_dogorqFilters = a}) . _Default;

-- | If specified, filters the results to include only options for the
-- specified major engine version.
dogorqMajorEngineVersion :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogorqMajorEngineVersion = lens _dogorqMajorEngineVersion (\ s a -> s{_dogorqMajorEngineVersion = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dogorqMaxRecords :: Lens' DescribeOptionGroupOptions (Maybe Int)
dogorqMaxRecords = lens _dogorqMaxRecords (\ s a -> s{_dogorqMaxRecords = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
dogorqMarker :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogorqMarker = lens _dogorqMarker (\ s a -> s{_dogorqMarker = a});

-- | A required parameter. Options available for the given Engine name will
-- be described.
dogorqEngineName :: Lens' DescribeOptionGroupOptions Text
dogorqEngineName = lens _dogorqEngineName (\ s a -> s{_dogorqEngineName = a});

instance AWSPager DescribeOptionGroupOptions where
        page rq rs
          | stop (rs ^. dogorsMarker) = Nothing
          | stop (rs ^. dogorsOptionGroupOptions) = Nothing
          | otherwise =
            Just $ rq & dogorqMarker .~ rs ^. dogorsMarker

instance AWSRequest DescribeOptionGroupOptions where
        type Sv DescribeOptionGroupOptions = RDS
        type Rs DescribeOptionGroupOptions =
             DescribeOptionGroupOptionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeOptionGroupOptionsResult"
              (\ s h x ->
                 DescribeOptionGroupOptionsResponse' <$>
                   (x .@? "OptionGroupOptions" .!@ mempty >>=
                      may (parseXMLList "OptionGroupOption"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeOptionGroupOptions where
        toHeaders = const mempty

instance ToPath DescribeOptionGroupOptions where
        toPath = const "/"

instance ToQuery DescribeOptionGroupOptions where
        toQuery DescribeOptionGroupOptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeOptionGroupOptions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dogorqFilters),
               "MajorEngineVersion" =: _dogorqMajorEngineVersion,
               "MaxRecords" =: _dogorqMaxRecords,
               "Marker" =: _dogorqMarker,
               "EngineName" =: _dogorqEngineName]

-- |
--
-- /See:/ 'describeOptionGroupOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogorsOptionGroupOptions'
--
-- * 'dogorsMarker'
--
-- * 'dogorsStatus'
data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse'
    { _dogorsOptionGroupOptions :: !(Maybe [OptionGroupOption])
    , _dogorsMarker             :: !(Maybe Text)
    , _dogorsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOptionGroupOptionsResponse' smart constructor.
describeOptionGroupOptionsResponse :: Int -> DescribeOptionGroupOptionsResponse
describeOptionGroupOptionsResponse pStatus =
    DescribeOptionGroupOptionsResponse'
    { _dogorsOptionGroupOptions = Nothing
    , _dogorsMarker = Nothing
    , _dogorsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dogorsOptionGroupOptions :: Lens' DescribeOptionGroupOptionsResponse [OptionGroupOption]
dogorsOptionGroupOptions = lens _dogorsOptionGroupOptions (\ s a -> s{_dogorsOptionGroupOptions = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
dogorsMarker :: Lens' DescribeOptionGroupOptionsResponse (Maybe Text)
dogorsMarker = lens _dogorsMarker (\ s a -> s{_dogorsMarker = a});

-- | FIXME: Undocumented member.
dogorsStatus :: Lens' DescribeOptionGroupOptionsResponse Int
dogorsStatus = lens _dogorsStatus (\ s a -> s{_dogorsStatus = a});
