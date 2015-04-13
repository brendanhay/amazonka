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

-- Module      : Network.AWS.MachineLearning.DescribeEvaluations
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

-- | Returns a list of 'DescribeEvaluations' that match the search criteria in the
-- request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeEvaluations.html>
module Network.AWS.MachineLearning.DescribeEvaluations
    (
    -- * Request
      DescribeEvaluations
    -- ** Request constructor
    , describeEvaluations
    -- ** Request lenses
    , deEQ
    , deFilterVariable
    , deGE
    , deGT
    , deLE
    , deLT
    , deLimit
    , deNE
    , deNextToken
    , dePrefix
    , deSortOrder

    -- * Response
    , DescribeEvaluationsResponse
    -- ** Response constructor
    , describeEvaluationsResponse
    -- ** Response lenses
    , derNextToken
    , derResults
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data DescribeEvaluations = DescribeEvaluations
    { _deEQ             :: Maybe Text
    , _deFilterVariable :: Maybe EvaluationFilterVariable
    , _deGE             :: Maybe Text
    , _deGT             :: Maybe Text
    , _deLE             :: Maybe Text
    , _deLT             :: Maybe Text
    , _deLimit          :: Maybe Nat
    , _deNE             :: Maybe Text
    , _deNextToken      :: Maybe Text
    , _dePrefix         :: Maybe Text
    , _deSortOrder      :: Maybe SortOrder
    } deriving (Eq, Read, Show)

-- | 'DescribeEvaluations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deEQ' @::@ 'Maybe' 'Text'
--
-- * 'deFilterVariable' @::@ 'Maybe' 'EvaluationFilterVariable'
--
-- * 'deGE' @::@ 'Maybe' 'Text'
--
-- * 'deGT' @::@ 'Maybe' 'Text'
--
-- * 'deLE' @::@ 'Maybe' 'Text'
--
-- * 'deLT' @::@ 'Maybe' 'Text'
--
-- * 'deLimit' @::@ 'Maybe' 'Natural'
--
-- * 'deNE' @::@ 'Maybe' 'Text'
--
-- * 'deNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dePrefix' @::@ 'Maybe' 'Text'
--
-- * 'deSortOrder' @::@ 'Maybe' 'SortOrder'
--
describeEvaluations :: DescribeEvaluations
describeEvaluations = DescribeEvaluations
    { _deFilterVariable = Nothing
    , _deEQ             = Nothing
    , _deGT             = Nothing
    , _deLT             = Nothing
    , _deGE             = Nothing
    , _deLE             = Nothing
    , _deNE             = Nothing
    , _dePrefix         = Nothing
    , _deSortOrder      = Nothing
    , _deNextToken      = Nothing
    , _deLimit          = Nothing
    }

-- | The equal to operator. The 'Evaluation' results will have 'FilterVariable' values
-- that exactly match the value specified with 'EQ'.
deEQ :: Lens' DescribeEvaluations (Maybe Text)
deEQ = lens _deEQ (\s a -> s { _deEQ = a })

-- | Use one of the following variable to filter a list of 'Evaluation' objects:
--
-- 'CreatedAt' - Sets the search criteria to the 'Evaluation' creation date.  'Status' - Sets the search criteria to the 'Evaluation' status.  'Name' - Sets the search
-- criteria to the contents of 'Evaluation'   'Name'.  'IAMUser' - Sets the search
-- criteria to the user account that invoked an 'Evaluation'.  'MLModelId' - Sets
-- the search criteria to the 'MLModel' that was evaluated.  'DataSourceId' - Sets
-- the search criteria to the 'DataSource' used in 'Evaluation'.  'DataUri' - Sets the
-- search criteria to the data file(s) used in 'Evaluation'. The URL can identify
-- either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or
-- directory.
deFilterVariable :: Lens' DescribeEvaluations (Maybe EvaluationFilterVariable)
deFilterVariable = lens _deFilterVariable (\s a -> s { _deFilterVariable = a })

-- | The greater than or equal to operator. The 'Evaluation' results will have 'FilterVariable' values that are greater than or equal to the value specified with 'GE'.
deGE :: Lens' DescribeEvaluations (Maybe Text)
deGE = lens _deGE (\s a -> s { _deGE = a })

-- | The greater than operator. The 'Evaluation' results will have 'FilterVariable'
-- values that are greater than the value specified with 'GT'.
deGT :: Lens' DescribeEvaluations (Maybe Text)
deGT = lens _deGT (\s a -> s { _deGT = a })

-- | The less than or equal to operator. The 'Evaluation' results will have 'FilterVariable' values that are less than or equal to the value specified with 'LE'.
deLE :: Lens' DescribeEvaluations (Maybe Text)
deLE = lens _deLE (\s a -> s { _deLE = a })

-- | The less than operator. The 'Evaluation' results will have 'FilterVariable'
-- values that are less than the value specified with 'LT'.
deLT :: Lens' DescribeEvaluations (Maybe Text)
deLT = lens _deLT (\s a -> s { _deLT = a })

-- | The maximum number of 'Evaluation' to include in the result.
deLimit :: Lens' DescribeEvaluations (Maybe Natural)
deLimit = lens _deLimit (\s a -> s { _deLimit = a }) . mapping _Nat

-- | The not equal to operator. The 'Evaluation' results will have 'FilterVariable'
-- values not equal to the value specified with 'NE'.
deNE :: Lens' DescribeEvaluations (Maybe Text)
deNE = lens _deNE (\s a -> s { _deNE = a })

-- | The ID of the page in the paginated results.
deNextToken :: Lens' DescribeEvaluations (Maybe Text)
deNextToken = lens _deNextToken (\s a -> s { _deNextToken = a })

-- | A string that is found at the beginning of a variable, such as 'Name' or 'Id'.
--
-- For example, an 'Evaluation' could have the 'Name' '2014-09-09-HolidayGiftMailer'.
-- To search for this 'Evaluation', select 'Name' for the 'FilterVariable' and any of
-- the following strings for the 'Prefix':
--
-- 2014-09
--
-- 2014-09-09
--
-- 2014-09-09-Holiday
--
--
dePrefix :: Lens' DescribeEvaluations (Maybe Text)
dePrefix = lens _dePrefix (\s a -> s { _dePrefix = a })

-- | A two-value parameter that determines the sequence of the resulting list of 'Evaluation'.
--
-- 'asc' - Arranges the list in ascending order (A-Z, 0-9).  'dsc' - Arranges the
-- list in descending order (Z-A, 9-0).  Results are sorted by 'FilterVariable'.
deSortOrder :: Lens' DescribeEvaluations (Maybe SortOrder)
deSortOrder = lens _deSortOrder (\s a -> s { _deSortOrder = a })

data DescribeEvaluationsResponse = DescribeEvaluationsResponse
    { _derNextToken :: Maybe Text
    , _derResults   :: List "Results" Evaluation
    } deriving (Eq, Read, Show)

-- | 'DescribeEvaluationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derNextToken' @::@ 'Maybe' 'Text'
--
-- * 'derResults' @::@ ['Evaluation']
--
describeEvaluationsResponse :: DescribeEvaluationsResponse
describeEvaluationsResponse = DescribeEvaluationsResponse
    { _derResults   = mempty
    , _derNextToken = Nothing
    }

-- | The ID of the next page in the paginated results that indicates at least one
-- more page follows.
derNextToken :: Lens' DescribeEvaluationsResponse (Maybe Text)
derNextToken = lens _derNextToken (\s a -> s { _derNextToken = a })

-- | A list of 'Evaluation' that meet the search criteria.
derResults :: Lens' DescribeEvaluationsResponse [Evaluation]
derResults = lens _derResults (\s a -> s { _derResults = a }) . _List

instance ToPath DescribeEvaluations where
    toPath = const "/"

instance ToQuery DescribeEvaluations where
    toQuery = const mempty

instance ToHeaders DescribeEvaluations

instance ToJSON DescribeEvaluations where
    toJSON DescribeEvaluations{..} = object
        [ "FilterVariable" .= _deFilterVariable
        , "EQ"             .= _deEQ
        , "GT"             .= _deGT
        , "LT"             .= _deLT
        , "GE"             .= _deGE
        , "LE"             .= _deLE
        , "NE"             .= _deNE
        , "Prefix"         .= _dePrefix
        , "SortOrder"      .= _deSortOrder
        , "NextToken"      .= _deNextToken
        , "Limit"          .= _deLimit
        ]

instance AWSRequest DescribeEvaluations where
    type Sv DescribeEvaluations = MachineLearning
    type Rs DescribeEvaluations = DescribeEvaluationsResponse

    request  = post "DescribeEvaluations"
    response = jsonResponse

instance FromJSON DescribeEvaluationsResponse where
    parseJSON = withObject "DescribeEvaluationsResponse" $ \o -> DescribeEvaluationsResponse
        <$> o .:? "NextToken"
        <*> o .:? "Results" .!= mempty

instance AWSPager DescribeEvaluations where
    page rq rs
        | stop (rs ^. derNextToken) = Nothing
        | otherwise = (\x -> rq & deNextToken ?~ x)
            <$> (rs ^. derNextToken)
