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

-- Module      : Network.AWS.MachineLearning.DescribeMLModels
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

-- | Returns a list of 'MLModel' that match the search criteria in the request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeMLModels.html>
module Network.AWS.MachineLearning.DescribeMLModels
    (
    -- * Request
      DescribeMLModels
    -- ** Request constructor
    , describeMLModels
    -- ** Request lenses
    , dmlmEQ
    , dmlmFilterVariable
    , dmlmGE
    , dmlmGT
    , dmlmLE
    , dmlmLT
    , dmlmLimit
    , dmlmNE
    , dmlmNextToken
    , dmlmPrefix
    , dmlmSortOrder

    -- * Response
    , DescribeMLModelsResponse
    -- ** Response constructor
    , describeMLModelsResponse
    -- ** Response lenses
    , dmlmrNextToken
    , dmlmrResults
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data DescribeMLModels = DescribeMLModels
    { _dmlmEQ             :: Maybe Text
    , _dmlmFilterVariable :: Maybe MLModelFilterVariable
    , _dmlmGE             :: Maybe Text
    , _dmlmGT             :: Maybe Text
    , _dmlmLE             :: Maybe Text
    , _dmlmLT             :: Maybe Text
    , _dmlmLimit          :: Maybe Nat
    , _dmlmNE             :: Maybe Text
    , _dmlmNextToken      :: Maybe Text
    , _dmlmPrefix         :: Maybe Text
    , _dmlmSortOrder      :: Maybe SortOrder
    } deriving (Eq, Read, Show)

-- | 'DescribeMLModels' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmEQ' @::@ 'Maybe' 'Text'
--
-- * 'dmlmFilterVariable' @::@ 'Maybe' 'MLModelFilterVariable'
--
-- * 'dmlmGE' @::@ 'Maybe' 'Text'
--
-- * 'dmlmGT' @::@ 'Maybe' 'Text'
--
-- * 'dmlmLE' @::@ 'Maybe' 'Text'
--
-- * 'dmlmLT' @::@ 'Maybe' 'Text'
--
-- * 'dmlmLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dmlmNE' @::@ 'Maybe' 'Text'
--
-- * 'dmlmNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dmlmPrefix' @::@ 'Maybe' 'Text'
--
-- * 'dmlmSortOrder' @::@ 'Maybe' 'SortOrder'
--
describeMLModels :: DescribeMLModels
describeMLModels = DescribeMLModels
    { _dmlmFilterVariable = Nothing
    , _dmlmEQ             = Nothing
    , _dmlmGT             = Nothing
    , _dmlmLT             = Nothing
    , _dmlmGE             = Nothing
    , _dmlmLE             = Nothing
    , _dmlmNE             = Nothing
    , _dmlmPrefix         = Nothing
    , _dmlmSortOrder      = Nothing
    , _dmlmNextToken      = Nothing
    , _dmlmLimit          = Nothing
    }

-- | The equal to operator. The 'MLModel' results will have 'FilterVariable' values
-- that exactly match the value specified with 'EQ'.
dmlmEQ :: Lens' DescribeMLModels (Maybe Text)
dmlmEQ = lens _dmlmEQ (\s a -> s { _dmlmEQ = a })

-- | Use one of the following variables to filter a list of 'MLModel':
--
-- 'CreatedAt' - Sets the search criteria to 'MLModel' creation date.  'Status' -
-- Sets the search criteria to 'MLModel' status.  'Name' - Sets the search criteria
-- to the contents of 'MLModel'  'Name'.  'IAMUser' - Sets the search criteria to the
-- user account that invoked the 'MLModel' creation.  'TrainingDataSourceId' - Sets
-- the search criteria to the 'DataSource' used to train one or more 'MLModel'.  'RealtimeEndpointStatus' - Sets the search criteria to the 'MLModel' real-time endpoint status.  'MLModelType' - Sets the search criteria to 'MLModel' type: binary, regression, or
-- multi-class.  'Algorithm' - Sets the search criteria to the algorithm that the 'MLModel' uses.  'TrainingDataURI' - Sets the search criteria to the data file(s) used
-- in training a 'MLModel'. The URL can identify either a file or an Amazon Simple
-- Storage Service (Amazon S3) bucket or directory.
dmlmFilterVariable :: Lens' DescribeMLModels (Maybe MLModelFilterVariable)
dmlmFilterVariable =
    lens _dmlmFilterVariable (\s a -> s { _dmlmFilterVariable = a })

-- | The greater than or equal to operator. The 'MLModel' results will have 'FilterVariable' values that are greater than or equal to the value specified with 'GE'.
dmlmGE :: Lens' DescribeMLModels (Maybe Text)
dmlmGE = lens _dmlmGE (\s a -> s { _dmlmGE = a })

-- | The greater than operator. The 'MLModel' results will have 'FilterVariable'
-- values that are greater than the value specified with 'GT'.
dmlmGT :: Lens' DescribeMLModels (Maybe Text)
dmlmGT = lens _dmlmGT (\s a -> s { _dmlmGT = a })

-- | The less than or equal to operator. The 'MLModel' results will have 'FilterVariable' values that are less than or equal to the value specified with 'LE'.
dmlmLE :: Lens' DescribeMLModels (Maybe Text)
dmlmLE = lens _dmlmLE (\s a -> s { _dmlmLE = a })

-- | The less than operator. The 'MLModel' results will have 'FilterVariable' values
-- that are less than the value specified with 'LT'.
dmlmLT :: Lens' DescribeMLModels (Maybe Text)
dmlmLT = lens _dmlmLT (\s a -> s { _dmlmLT = a })

-- | The number of pages of information to include in the result. The range of
-- acceptable values is 1 through 100. The default value is 100.
dmlmLimit :: Lens' DescribeMLModels (Maybe Natural)
dmlmLimit = lens _dmlmLimit (\s a -> s { _dmlmLimit = a }) . mapping _Nat

-- | The not equal to operator. The 'MLModel' results will have 'FilterVariable'
-- values not equal to the value specified with 'NE'.
dmlmNE :: Lens' DescribeMLModels (Maybe Text)
dmlmNE = lens _dmlmNE (\s a -> s { _dmlmNE = a })

-- | The ID of the page in the paginated results.
dmlmNextToken :: Lens' DescribeMLModels (Maybe Text)
dmlmNextToken = lens _dmlmNextToken (\s a -> s { _dmlmNextToken = a })

-- | A string that is found at the beginning of a variable, such as 'Name' or 'Id'.
--
-- For example, an 'MLModel' could have the 'Name' '2014-09-09-HolidayGiftMailer'. To
-- search for this 'MLModel', select 'Name' for the 'FilterVariable' and any of the
-- following strings for the 'Prefix':
--
-- 2014-09
--
-- 2014-09-09
--
-- 2014-09-09-Holiday
--
--
dmlmPrefix :: Lens' DescribeMLModels (Maybe Text)
dmlmPrefix = lens _dmlmPrefix (\s a -> s { _dmlmPrefix = a })

-- | A two-value parameter that determines the sequence of the resulting list of 'MLModel'.
--
-- 'asc' - Arranges the list in ascending order (A-Z, 0-9).  'dsc' - Arranges the
-- list in descending order (Z-A, 9-0).  Results are sorted by 'FilterVariable'.
dmlmSortOrder :: Lens' DescribeMLModels (Maybe SortOrder)
dmlmSortOrder = lens _dmlmSortOrder (\s a -> s { _dmlmSortOrder = a })

data DescribeMLModelsResponse = DescribeMLModelsResponse
    { _dmlmrNextToken :: Maybe Text
    , _dmlmrResults   :: List "Results" MLModel
    } deriving (Eq, Read, Show)

-- | 'DescribeMLModelsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dmlmrResults' @::@ ['MLModel']
--
describeMLModelsResponse :: DescribeMLModelsResponse
describeMLModelsResponse = DescribeMLModelsResponse
    { _dmlmrResults   = mempty
    , _dmlmrNextToken = Nothing
    }

-- | The ID of the next page in the paginated results that indicates at least one
-- more page follows.
dmlmrNextToken :: Lens' DescribeMLModelsResponse (Maybe Text)
dmlmrNextToken = lens _dmlmrNextToken (\s a -> s { _dmlmrNextToken = a })

-- | A list of 'MLModel' that meet the search criteria.
dmlmrResults :: Lens' DescribeMLModelsResponse [MLModel]
dmlmrResults = lens _dmlmrResults (\s a -> s { _dmlmrResults = a }) . _List

instance ToPath DescribeMLModels where
    toPath = const "/"

instance ToQuery DescribeMLModels where
    toQuery = const mempty

instance ToHeaders DescribeMLModels

instance ToJSON DescribeMLModels where
    toJSON DescribeMLModels{..} = object
        [ "FilterVariable" .= _dmlmFilterVariable
        , "EQ"             .= _dmlmEQ
        , "GT"             .= _dmlmGT
        , "LT"             .= _dmlmLT
        , "GE"             .= _dmlmGE
        , "LE"             .= _dmlmLE
        , "NE"             .= _dmlmNE
        , "Prefix"         .= _dmlmPrefix
        , "SortOrder"      .= _dmlmSortOrder
        , "NextToken"      .= _dmlmNextToken
        , "Limit"          .= _dmlmLimit
        ]

instance AWSRequest DescribeMLModels where
    type Sv DescribeMLModels = MachineLearning
    type Rs DescribeMLModels = DescribeMLModelsResponse

    request  = post "DescribeMLModels"
    response = jsonResponse

instance FromJSON DescribeMLModelsResponse where
    parseJSON = withObject "DescribeMLModelsResponse" $ \o -> DescribeMLModelsResponse
        <$> o .:? "NextToken"
        <*> o .:? "Results" .!= mempty

instance AWSPager DescribeMLModels where
    page rq rs
        | stop (rs ^. dmlmrNextToken) = Nothing
        | otherwise = (\x -> rq & dmlmNextToken ?~ x)
            <$> (rs ^. dmlmrNextToken)
