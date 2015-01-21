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

-- Module      : Network.AWS.CloudHSM.DescribeHapg
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

-- | Retrieves information about a high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeHapg.html>
module Network.AWS.CloudHSM.DescribeHapg
    (
    -- * Request
      DescribeHapg
    -- ** Request constructor
    , describeHapg
    -- ** Request lenses
    , dh1HapgArn

    -- * Response
    , DescribeHapgResponse
    -- ** Response constructor
    , describeHapgResponse
    -- ** Response lenses
    , dhrHapgArn
    , dhrHapgSerial
    , dhrHsmsLastActionFailed
    , dhrHsmsPendingDeletion
    , dhrHsmsPendingRegistration
    , dhrLabel
    , dhrLastModifiedTimestamp
    , dhrPartitionSerialList
    , dhrState
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype DescribeHapg = DescribeHapg
    { _dh1HapgArn :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeHapg' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dh1HapgArn' @::@ 'Text'
--
describeHapg :: Text -- ^ 'dh1HapgArn'
             -> DescribeHapg
describeHapg p1 = DescribeHapg
    { _dh1HapgArn = p1
    }

-- | The ARN of the high-availability partition group to describe.
dh1HapgArn :: Lens' DescribeHapg Text
dh1HapgArn = lens _dh1HapgArn (\s a -> s { _dh1HapgArn = a })

data DescribeHapgResponse = DescribeHapgResponse
    { _dhrHapgArn                 :: Maybe Text
    , _dhrHapgSerial              :: Maybe Text
    , _dhrHsmsLastActionFailed    :: List "HsmsLastActionFailed" Text
    , _dhrHsmsPendingDeletion     :: List "HsmsPendingDeletion" Text
    , _dhrHsmsPendingRegistration :: List "HsmsPendingRegistration" Text
    , _dhrLabel                   :: Maybe Text
    , _dhrLastModifiedTimestamp   :: Maybe Text
    , _dhrPartitionSerialList     :: List "PartitionSerialList" Text
    , _dhrState                   :: Maybe CloudHsmObjectState
    } deriving (Eq, Read, Show)

-- | 'DescribeHapgResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrHapgArn' @::@ 'Maybe' 'Text'
--
-- * 'dhrHapgSerial' @::@ 'Maybe' 'Text'
--
-- * 'dhrHsmsLastActionFailed' @::@ ['Text']
--
-- * 'dhrHsmsPendingDeletion' @::@ ['Text']
--
-- * 'dhrHsmsPendingRegistration' @::@ ['Text']
--
-- * 'dhrLabel' @::@ 'Maybe' 'Text'
--
-- * 'dhrLastModifiedTimestamp' @::@ 'Maybe' 'Text'
--
-- * 'dhrPartitionSerialList' @::@ ['Text']
--
-- * 'dhrState' @::@ 'Maybe' 'CloudHsmObjectState'
--
describeHapgResponse :: DescribeHapgResponse
describeHapgResponse = DescribeHapgResponse
    { _dhrHapgArn                 = Nothing
    , _dhrHapgSerial              = Nothing
    , _dhrHsmsLastActionFailed    = mempty
    , _dhrHsmsPendingDeletion     = mempty
    , _dhrHsmsPendingRegistration = mempty
    , _dhrLabel                   = Nothing
    , _dhrLastModifiedTimestamp   = Nothing
    , _dhrPartitionSerialList     = mempty
    , _dhrState                   = Nothing
    }

-- | The ARN of the high-availability partition group.
dhrHapgArn :: Lens' DescribeHapgResponse (Maybe Text)
dhrHapgArn = lens _dhrHapgArn (\s a -> s { _dhrHapgArn = a })

-- | The serial number of the high-availability partition group.
dhrHapgSerial :: Lens' DescribeHapgResponse (Maybe Text)
dhrHapgSerial = lens _dhrHapgSerial (\s a -> s { _dhrHapgSerial = a })

dhrHsmsLastActionFailed :: Lens' DescribeHapgResponse [Text]
dhrHsmsLastActionFailed =
    lens _dhrHsmsLastActionFailed (\s a -> s { _dhrHsmsLastActionFailed = a })
        . _List

dhrHsmsPendingDeletion :: Lens' DescribeHapgResponse [Text]
dhrHsmsPendingDeletion =
    lens _dhrHsmsPendingDeletion (\s a -> s { _dhrHsmsPendingDeletion = a })
        . _List

dhrHsmsPendingRegistration :: Lens' DescribeHapgResponse [Text]
dhrHsmsPendingRegistration =
    lens _dhrHsmsPendingRegistration
        (\s a -> s { _dhrHsmsPendingRegistration = a })
            . _List

-- | The label for the high-availability partition group.
dhrLabel :: Lens' DescribeHapgResponse (Maybe Text)
dhrLabel = lens _dhrLabel (\s a -> s { _dhrLabel = a })

-- | The date and time the high-availability partition group was last modified.
dhrLastModifiedTimestamp :: Lens' DescribeHapgResponse (Maybe Text)
dhrLastModifiedTimestamp =
    lens _dhrLastModifiedTimestamp
        (\s a -> s { _dhrLastModifiedTimestamp = a })

-- | The list of partition serial numbers that belong to the high-availability
-- partition group.
dhrPartitionSerialList :: Lens' DescribeHapgResponse [Text]
dhrPartitionSerialList =
    lens _dhrPartitionSerialList (\s a -> s { _dhrPartitionSerialList = a })
        . _List

-- | The state of the high-availability partition group.
dhrState :: Lens' DescribeHapgResponse (Maybe CloudHsmObjectState)
dhrState = lens _dhrState (\s a -> s { _dhrState = a })

instance ToPath DescribeHapg where
    toPath = const "/"

instance ToQuery DescribeHapg where
    toQuery = const mempty

instance ToHeaders DescribeHapg

instance ToJSON DescribeHapg where
    toJSON DescribeHapg{..} = object
        [ "HapgArn" .= _dh1HapgArn
        ]

instance AWSRequest DescribeHapg where
    type Sv DescribeHapg = CloudHSM
    type Rs DescribeHapg = DescribeHapgResponse

    request  = post "DescribeHapg"
    response = jsonResponse

instance FromJSON DescribeHapgResponse where
    parseJSON = withObject "DescribeHapgResponse" $ \o -> DescribeHapgResponse
        <$> o .:? "HapgArn"
        <*> o .:? "HapgSerial"
        <*> o .:? "HsmsLastActionFailed" .!= mempty
        <*> o .:? "HsmsPendingDeletion" .!= mempty
        <*> o .:? "HsmsPendingRegistration" .!= mempty
        <*> o .:? "Label"
        <*> o .:? "LastModifiedTimestamp"
        <*> o .:? "PartitionSerialList" .!= mempty
        <*> o .:? "State"
