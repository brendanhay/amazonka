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
-- Module      : Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases the Amazon Kinesis stream\'s retention period, which is the length of time data records are accessible after they are added to the stream. The maximum value of a stream\'s retention period is 168 hours (7 days).
--
-- Upon choosing a longer stream retention period, this operation will increase the time period records are accessible that have not yet expired. However, it will not make previous data that has expired (older than the stream\'s previous retention period) accessible after the operation has been called. For example, if a stream\'s retention period is set to 24 hours and is increased to 168 hours, any data that is older than 24 hours will remain inaccessible to consumer applications.
module Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
    (
    -- * Creating a Request
      increaseStreamRetentionPeriod
    , IncreaseStreamRetentionPeriod
    -- * Request Lenses
    , isrpStreamName
    , isrpRetentionPeriodHours

    -- * Destructuring the Response
    , increaseStreamRetentionPeriodResponse
    , IncreaseStreamRetentionPeriodResponse
    ) where

import           Network.AWS.Kinesis.Types
import           Network.AWS.Kinesis.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for < IncreaseStreamRetentionPeriod>.
--
-- /See:/ 'increaseStreamRetentionPeriod' smart constructor.
data IncreaseStreamRetentionPeriod = IncreaseStreamRetentionPeriod'
    { _isrpStreamName           :: !Text
    , _isrpRetentionPeriodHours :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IncreaseStreamRetentionPeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrpStreamName'
--
-- * 'isrpRetentionPeriodHours'
increaseStreamRetentionPeriod
    :: Text -- ^ 'isrpStreamName'
    -> Natural -- ^ 'isrpRetentionPeriodHours'
    -> IncreaseStreamRetentionPeriod
increaseStreamRetentionPeriod pStreamName_ pRetentionPeriodHours_ =
    IncreaseStreamRetentionPeriod'
    { _isrpStreamName = pStreamName_
    , _isrpRetentionPeriodHours = _Nat # pRetentionPeriodHours_
    }

-- | The name of the stream to modify.
isrpStreamName :: Lens' IncreaseStreamRetentionPeriod Text
isrpStreamName = lens _isrpStreamName (\ s a -> s{_isrpStreamName = a});

-- | The new retention period of the stream, in hours. Must be more than the current retention period.
isrpRetentionPeriodHours :: Lens' IncreaseStreamRetentionPeriod Natural
isrpRetentionPeriodHours = lens _isrpRetentionPeriodHours (\ s a -> s{_isrpRetentionPeriodHours = a}) . _Nat;

instance AWSRequest IncreaseStreamRetentionPeriod
         where
        type Rs IncreaseStreamRetentionPeriod =
             IncreaseStreamRetentionPeriodResponse
        request = postJSON kinesis
        response
          = receiveNull IncreaseStreamRetentionPeriodResponse'

instance Hashable IncreaseStreamRetentionPeriod

instance NFData IncreaseStreamRetentionPeriod

instance ToHeaders IncreaseStreamRetentionPeriod
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.IncreaseStreamRetentionPeriod" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON IncreaseStreamRetentionPeriod where
        toJSON IncreaseStreamRetentionPeriod'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _isrpStreamName),
                  Just
                    ("RetentionPeriodHours" .=
                       _isrpRetentionPeriodHours)])

instance ToPath IncreaseStreamRetentionPeriod where
        toPath = const "/"

instance ToQuery IncreaseStreamRetentionPeriod where
        toQuery = const mempty

-- | /See:/ 'increaseStreamRetentionPeriodResponse' smart constructor.
data IncreaseStreamRetentionPeriodResponse =
    IncreaseStreamRetentionPeriodResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IncreaseStreamRetentionPeriodResponse' with the minimum fields required to make a request.
--
increaseStreamRetentionPeriodResponse
    :: IncreaseStreamRetentionPeriodResponse
increaseStreamRetentionPeriodResponse = IncreaseStreamRetentionPeriodResponse'

instance NFData IncreaseStreamRetentionPeriodResponse
