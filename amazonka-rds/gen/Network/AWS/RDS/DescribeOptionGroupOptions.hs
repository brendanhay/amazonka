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
    , dogoFilters
    , dogoMajorEngineVersion
    , dogoMaxRecords
    , dogoMarker
    , dogoEngineName

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
-- * 'dogoFilters'
--
-- * 'dogoMajorEngineVersion'
--
-- * 'dogoMaxRecords'
--
-- * 'dogoMarker'
--
-- * 'dogoEngineName'
data DescribeOptionGroupOptions = DescribeOptionGroupOptions'
    { _dogoFilters            :: !(Maybe [Filter])
    , _dogoMajorEngineVersion :: !(Maybe Text)
    , _dogoMaxRecords         :: !(Maybe Int)
    , _dogoMarker             :: !(Maybe Text)
    , _dogoEngineName         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOptionGroupOptions' smart constructor.
describeOptionGroupOptions :: Text -> DescribeOptionGroupOptions
describeOptionGroupOptions pEngineName_ =
    DescribeOptionGroupOptions'
    { _dogoFilters = Nothing
    , _dogoMajorEngineVersion = Nothing
    , _dogoMaxRecords = Nothing
    , _dogoMarker = Nothing
    , _dogoEngineName = pEngineName_
    }

-- | This parameter is not currently supported.
dogoFilters :: Lens' DescribeOptionGroupOptions [Filter]
dogoFilters = lens _dogoFilters (\ s a -> s{_dogoFilters = a}) . _Default . _Coerce;

-- | If specified, filters the results to include only options for the
-- specified major engine version.
dogoMajorEngineVersion :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogoMajorEngineVersion = lens _dogoMajorEngineVersion (\ s a -> s{_dogoMajorEngineVersion = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dogoMaxRecords :: Lens' DescribeOptionGroupOptions (Maybe Int)
dogoMaxRecords = lens _dogoMaxRecords (\ s a -> s{_dogoMaxRecords = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
dogoMarker :: Lens' DescribeOptionGroupOptions (Maybe Text)
dogoMarker = lens _dogoMarker (\ s a -> s{_dogoMarker = a});

-- | A required parameter. Options available for the given Engine name will
-- be described.
dogoEngineName :: Lens' DescribeOptionGroupOptions Text
dogoEngineName = lens _dogoEngineName (\ s a -> s{_dogoEngineName = a});

instance AWSPager DescribeOptionGroupOptions where
        page rq rs
          | stop (rs ^. dogorsMarker) = Nothing
          | stop (rs ^. dogorsOptionGroupOptions) = Nothing
          | otherwise =
            Just $ rq & dogoMarker .~ rs ^. dogorsMarker

instance AWSRequest DescribeOptionGroupOptions where
        type Sv DescribeOptionGroupOptions = RDS
        type Rs DescribeOptionGroupOptions =
             DescribeOptionGroupOptionsResponse
        request = postQuery
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
        toPath = const mempty

instance ToQuery DescribeOptionGroupOptions where
        toQuery DescribeOptionGroupOptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeOptionGroupOptions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dogoFilters),
               "MajorEngineVersion" =: _dogoMajorEngineVersion,
               "MaxRecords" =: _dogoMaxRecords,
               "Marker" =: _dogoMarker,
               "EngineName" =: _dogoEngineName]

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
describeOptionGroupOptionsResponse pStatus_ =
    DescribeOptionGroupOptionsResponse'
    { _dogorsOptionGroupOptions = Nothing
    , _dogorsMarker = Nothing
    , _dogorsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dogorsOptionGroupOptions :: Lens' DescribeOptionGroupOptionsResponse [OptionGroupOption]
dogorsOptionGroupOptions = lens _dogorsOptionGroupOptions (\ s a -> s{_dogorsOptionGroupOptions = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
dogorsMarker :: Lens' DescribeOptionGroupOptionsResponse (Maybe Text)
dogorsMarker = lens _dogorsMarker (\ s a -> s{_dogorsMarker = a});

-- | FIXME: Undocumented member.
dogorsStatus :: Lens' DescribeOptionGroupOptionsResponse Int
dogorsStatus = lens _dogorsStatus (\ s a -> s{_dogorsStatus = a});
