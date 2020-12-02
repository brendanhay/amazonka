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
-- Module      : Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The minimum value of a stream's retention period is 24 hours.
--
--
-- This operation may result in lost data. For example, if the stream's retention period is 48 hours and is decreased to 24 hours, any data already in the stream that is older than 24 hours is inaccessible.
--
module Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
    (
    -- * Creating a Request
      decreaseStreamRetentionPeriod
    , DecreaseStreamRetentionPeriod
    -- * Request Lenses
    , dsrpStreamName
    , dsrpRetentionPeriodHours

    -- * Destructuring the Response
    , decreaseStreamRetentionPeriodResponse
    , DecreaseStreamRetentionPeriodResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for 'DecreaseStreamRetentionPeriod' .
--
--
--
-- /See:/ 'decreaseStreamRetentionPeriod' smart constructor.
data DecreaseStreamRetentionPeriod = DecreaseStreamRetentionPeriod'
  { _dsrpStreamName           :: !Text
  , _dsrpRetentionPeriodHours :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecreaseStreamRetentionPeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrpStreamName' - The name of the stream to modify.
--
-- * 'dsrpRetentionPeriodHours' - The new retention period of the stream, in hours. Must be less than the current retention period.
decreaseStreamRetentionPeriod
    :: Text -- ^ 'dsrpStreamName'
    -> Natural -- ^ 'dsrpRetentionPeriodHours'
    -> DecreaseStreamRetentionPeriod
decreaseStreamRetentionPeriod pStreamName_ pRetentionPeriodHours_ =
  DecreaseStreamRetentionPeriod'
    { _dsrpStreamName = pStreamName_
    , _dsrpRetentionPeriodHours = _Nat # pRetentionPeriodHours_
    }


-- | The name of the stream to modify.
dsrpStreamName :: Lens' DecreaseStreamRetentionPeriod Text
dsrpStreamName = lens _dsrpStreamName (\ s a -> s{_dsrpStreamName = a})

-- | The new retention period of the stream, in hours. Must be less than the current retention period.
dsrpRetentionPeriodHours :: Lens' DecreaseStreamRetentionPeriod Natural
dsrpRetentionPeriodHours = lens _dsrpRetentionPeriodHours (\ s a -> s{_dsrpRetentionPeriodHours = a}) . _Nat

instance AWSRequest DecreaseStreamRetentionPeriod
         where
        type Rs DecreaseStreamRetentionPeriod =
             DecreaseStreamRetentionPeriodResponse
        request = postJSON kinesis
        response
          = receiveNull DecreaseStreamRetentionPeriodResponse'

instance Hashable DecreaseStreamRetentionPeriod where

instance NFData DecreaseStreamRetentionPeriod where

instance ToHeaders DecreaseStreamRetentionPeriod
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DecreaseStreamRetentionPeriod" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DecreaseStreamRetentionPeriod where
        toJSON DecreaseStreamRetentionPeriod'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _dsrpStreamName),
                  Just
                    ("RetentionPeriodHours" .=
                       _dsrpRetentionPeriodHours)])

instance ToPath DecreaseStreamRetentionPeriod where
        toPath = const "/"

instance ToQuery DecreaseStreamRetentionPeriod where
        toQuery = const mempty

-- | /See:/ 'decreaseStreamRetentionPeriodResponse' smart constructor.
data DecreaseStreamRetentionPeriodResponse =
  DecreaseStreamRetentionPeriodResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecreaseStreamRetentionPeriodResponse' with the minimum fields required to make a request.
--
decreaseStreamRetentionPeriodResponse
    :: DecreaseStreamRetentionPeriodResponse
decreaseStreamRetentionPeriodResponse = DecreaseStreamRetentionPeriodResponse'


instance NFData DecreaseStreamRetentionPeriodResponse
         where
