{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.DescribeBatchPredictions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of 'BatchPrediction' operations that match the search criteria
-- in the request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeBatchPredictions.html>
module Network.AWS.MachineLearning.DescribeBatchPredictions
    (
    -- * Request
      DescribeBatchPredictions
    -- ** Request constructor
    , describeBatchPredictions
    -- ** Request lenses
    , dbpEQ
    , dbpFilterVariable
    , dbpGE
    , dbpGT
    , dbpLE
    , dbpLT
    , dbpLimit
    , dbpNE
    , dbpNextToken
    , dbpPrefix
    , dbpSortOrder

    -- * Response
    , DescribeBatchPredictionsResponse
    -- ** Response constructor
    , describeBatchPredictionsResponse
    -- ** Response lenses
    , dbprNextToken
    , dbprResults
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data DescribeBatchPredictions = DescribeBatchPredictions
    { _dbpEQ             :: Maybe Text
    , _dbpFilterVariable :: Maybe BatchPredictionFilterVariable
    , _dbpGE             :: Maybe Text
    , _dbpGT             :: Maybe Text
    , _dbpLE             :: Maybe Text
    , _dbpLT             :: Maybe Text
    , _dbpLimit          :: Maybe Nat
    , _dbpNE             :: Maybe Text
    , _dbpNextToken      :: Maybe Text
    , _dbpPrefix         :: Maybe Text
    , _dbpSortOrder      :: Maybe SortOrder
    } deriving (Eq, Read, Show)

-- | 'DescribeBatchPredictions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpEQ' @::@ 'Maybe' 'Text'
--
-- * 'dbpFilterVariable' @::@ 'Maybe' 'BatchPredictionFilterVariable'
--
-- * 'dbpGE' @::@ 'Maybe' 'Text'
--
-- * 'dbpGT' @::@ 'Maybe' 'Text'
--
-- * 'dbpLE' @::@ 'Maybe' 'Text'
--
-- * 'dbpLT' @::@ 'Maybe' 'Text'
--
-- * 'dbpLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dbpNE' @::@ 'Maybe' 'Text'
--
-- * 'dbpNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dbpPrefix' @::@ 'Maybe' 'Text'
--
-- * 'dbpSortOrder' @::@ 'Maybe' 'SortOrder'
--
describeBatchPredictions :: DescribeBatchPredictions
describeBatchPredictions = DescribeBatchPredictions
    { _dbpFilterVariable = Nothing
    , _dbpEQ             = Nothing
    , _dbpGT             = Nothing
    , _dbpLT             = Nothing
    , _dbpGE             = Nothing
    , _dbpLE             = Nothing
    , _dbpNE             = Nothing
    , _dbpPrefix         = Nothing
    , _dbpSortOrder      = Nothing
    , _dbpNextToken      = Nothing
    , _dbpLimit          = Nothing
    }

-- | The equal to operator. The 'BatchPrediction' results will have 'FilterVariable'
-- values that exactly match the value specified with 'EQ'.
dbpEQ :: Lens' DescribeBatchPredictions (Maybe Text)
dbpEQ = lens _dbpEQ (\s a -> s { _dbpEQ = a })

-- | Use one of the following variables to filter a list of 'BatchPrediction':
--
-- 'CreatedAt' - Sets the search criteria to the 'BatchPrediction' creation date.
-- 'Status' - Sets the search criteria to the 'BatchPrediction' status.  'Name' -
-- Sets the search criteria to the contents of the 'BatchPrediction'  'Name'.  'IAMUser' - Sets the search criteria to the user account that invoked the 'BatchPrediction' creation.  'MLModelId' - Sets the search criteria to the 'MLModel' used in the 'BatchPrediction'.  'DataSourceId' - Sets the search criteria to the 'DataSource' used in the 'BatchPrediction'.  'DataURI' - Sets the search criteria to the data file(s) used in the 'BatchPrediction'. The URL can identify either a file or an Amazon Simple Storage Solution
-- (Amazon S3) bucket or directory.
dbpFilterVariable :: Lens' DescribeBatchPredictions (Maybe BatchPredictionFilterVariable)
dbpFilterVariable =
    lens _dbpFilterVariable (\s a -> s { _dbpFilterVariable = a })

-- | The greater than or equal to operator. The 'BatchPrediction' results will have 'FilterVariable' values that are greater than or equal to the value specified with 'GE'.
dbpGE :: Lens' DescribeBatchPredictions (Maybe Text)
dbpGE = lens _dbpGE (\s a -> s { _dbpGE = a })

-- | The greater than operator. The 'BatchPrediction' results will have 'FilterVariable' values that are greater than the value specified with 'GT'.
dbpGT :: Lens' DescribeBatchPredictions (Maybe Text)
dbpGT = lens _dbpGT (\s a -> s { _dbpGT = a })

-- | The less than or equal to operator. The 'BatchPrediction' results will have 'FilterVariable' values that are less than or equal to the value specified with 'LE'.
dbpLE :: Lens' DescribeBatchPredictions (Maybe Text)
dbpLE = lens _dbpLE (\s a -> s { _dbpLE = a })

-- | The less than operator. The 'BatchPrediction' results will have 'FilterVariable'
-- values that are less than the value specified with 'LT'.
dbpLT :: Lens' DescribeBatchPredictions (Maybe Text)
dbpLT = lens _dbpLT (\s a -> s { _dbpLT = a })

-- | The number of pages of information to include in the result. The range of
-- acceptable values is 1 through 100. The default value is 100.
dbpLimit :: Lens' DescribeBatchPredictions (Maybe Natural)
dbpLimit = lens _dbpLimit (\s a -> s { _dbpLimit = a }) . mapping _Nat

-- | The not equal to operator. The 'BatchPrediction' results will have 'FilterVariable' values not equal to the value specified with 'NE'.
dbpNE :: Lens' DescribeBatchPredictions (Maybe Text)
dbpNE = lens _dbpNE (\s a -> s { _dbpNE = a })

-- | An ID of the page in the paginated results.
dbpNextToken :: Lens' DescribeBatchPredictions (Maybe Text)
dbpNextToken = lens _dbpNextToken (\s a -> s { _dbpNextToken = a })

-- | A string that is found at the beginning of a variable, such as 'Name' or 'Id'.
--
-- For example, a 'Batch Prediction' operation could have the 'Name' '2014-09-09-HolidayGiftMailer'. To search for this 'BatchPrediction', select 'Name' for the 'FilterVariable' and
-- any of the following strings for the 'Prefix':
--
-- 2014-09
--
-- 2014-09-09
--
-- 2014-09-09-Holiday
--
--
dbpPrefix :: Lens' DescribeBatchPredictions (Maybe Text)
dbpPrefix = lens _dbpPrefix (\s a -> s { _dbpPrefix = a })

-- | A two-value parameter that determines the sequence of the resulting list of 'MLModel's.
--
-- 'asc' - Arranges the list in ascending order (A-Z, 0-9).  'dsc' - Arranges the
-- list in descending order (Z-A, 9-0).  Results are sorted by 'FilterVariable'.
dbpSortOrder :: Lens' DescribeBatchPredictions (Maybe SortOrder)
dbpSortOrder = lens _dbpSortOrder (\s a -> s { _dbpSortOrder = a })

data DescribeBatchPredictionsResponse = DescribeBatchPredictionsResponse
    { _dbprNextToken :: Maybe Text
    , _dbprResults   :: List "Results" BatchPrediction
    } deriving (Eq, Read, Show)

-- | 'DescribeBatchPredictionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbprNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dbprResults' @::@ ['BatchPrediction']
--
describeBatchPredictionsResponse :: DescribeBatchPredictionsResponse
describeBatchPredictionsResponse = DescribeBatchPredictionsResponse
    { _dbprResults   = mempty
    , _dbprNextToken = Nothing
    }

-- | The ID of the next page in the paginated results that indicates at least one
-- more page follows.
dbprNextToken :: Lens' DescribeBatchPredictionsResponse (Maybe Text)
dbprNextToken = lens _dbprNextToken (\s a -> s { _dbprNextToken = a })

-- | A list of 'BatchPrediction' objects that meet the search criteria.
dbprResults :: Lens' DescribeBatchPredictionsResponse [BatchPrediction]
dbprResults = lens _dbprResults (\s a -> s { _dbprResults = a }) . _List

instance ToPath DescribeBatchPredictions where
    toPath = const "/"

instance ToQuery DescribeBatchPredictions where
    toQuery = const mempty

instance ToHeaders DescribeBatchPredictions

instance ToJSON DescribeBatchPredictions where
    toJSON DescribeBatchPredictions{..} = object
        [ "FilterVariable" .= _dbpFilterVariable
        , "EQ"             .= _dbpEQ
        , "GT"             .= _dbpGT
        , "LT"             .= _dbpLT
        , "GE"             .= _dbpGE
        , "LE"             .= _dbpLE
        , "NE"             .= _dbpNE
        , "Prefix"         .= _dbpPrefix
        , "SortOrder"      .= _dbpSortOrder
        , "NextToken"      .= _dbpNextToken
        , "Limit"          .= _dbpLimit
        ]

instance AWSRequest DescribeBatchPredictions where
    type Sv DescribeBatchPredictions = MachineLearning
    type Rs DescribeBatchPredictions = DescribeBatchPredictionsResponse

    request  = post "DescribeBatchPredictions"
    response = jsonResponse

instance FromJSON DescribeBatchPredictionsResponse where
    parseJSON = withObject "DescribeBatchPredictionsResponse" $ \o -> DescribeBatchPredictionsResponse
        <$> o .:? "NextToken"
        <*> o .:? "Results" .!= mempty

instance AWSPager DescribeBatchPredictions where
    page rq rs
        | stop (rs ^. dbprNextToken) = Nothing
        | otherwise = (\x -> rq & dbpNextToken ?~ x)
            <$> (rs ^. dbprNextToken)
