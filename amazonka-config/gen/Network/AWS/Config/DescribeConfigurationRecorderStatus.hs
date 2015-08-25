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
-- Module      : Network.AWS.Config.DescribeConfigurationRecorderStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified configuration recorder. If a
-- configuration recorder is not specified, this action returns the status
-- of all configuration recorder associated with the account.
--
-- Currently, you can specify only one configuration recorder per account.
--
-- /See:/ <http://docs.aws.amazon.com/config/latest/APIReference/API_DescribeConfigurationRecorderStatus.html AWS API Reference> for DescribeConfigurationRecorderStatus.
module Network.AWS.Config.DescribeConfigurationRecorderStatus
    (
    -- * Creating a Request
      describeConfigurationRecorderStatus
    , DescribeConfigurationRecorderStatus
    -- * Request Lenses
    , dcrsConfigurationRecorderNames

    -- * Destructuring the Response
    , describeConfigurationRecorderStatusResponse
    , DescribeConfigurationRecorderStatusResponse
    -- * Response Lenses
    , dcrsrsConfigurationRecordersStatus
    , dcrsrsStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Config.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeConfigurationRecorderStatus action.
--
-- /See:/ 'describeConfigurationRecorderStatus' smart constructor.
newtype DescribeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'
    { _dcrsConfigurationRecorderNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigurationRecorderStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsConfigurationRecorderNames'
describeConfigurationRecorderStatus
    :: DescribeConfigurationRecorderStatus
describeConfigurationRecorderStatus =
    DescribeConfigurationRecorderStatus'
    { _dcrsConfigurationRecorderNames = Nothing
    }

-- | The name(s) of the configuration recorder. If the name is not specified,
-- the action returns the current status of all the configuration recorders
-- associated with the account.
dcrsConfigurationRecorderNames :: Lens' DescribeConfigurationRecorderStatus [Text]
dcrsConfigurationRecorderNames = lens _dcrsConfigurationRecorderNames (\ s a -> s{_dcrsConfigurationRecorderNames = a}) . _Default . _Coerce;

instance AWSRequest
         DescribeConfigurationRecorderStatus where
        type Rs DescribeConfigurationRecorderStatus =
             DescribeConfigurationRecorderStatusResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationRecorderStatusResponse' <$>
                   (x .?> "ConfigurationRecordersStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders
         DescribeConfigurationRecorderStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigurationRecorderStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigurationRecorderStatus
         where
        toJSON DescribeConfigurationRecorderStatus'{..}
          = object
              (catMaybes
                 [("ConfigurationRecorderNames" .=) <$>
                    _dcrsConfigurationRecorderNames])

instance ToPath DescribeConfigurationRecorderStatus
         where
        toPath = const "/"

instance ToQuery DescribeConfigurationRecorderStatus
         where
        toQuery = const mempty

-- | The output for the DescribeConfigurationRecorderStatus action in JSON
-- format.
--
-- /See:/ 'describeConfigurationRecorderStatusResponse' smart constructor.
data DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'
    { _dcrsrsConfigurationRecordersStatus :: !(Maybe [ConfigurationRecorderStatus])
    , _dcrsrsStatus                       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigurationRecorderStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsrsConfigurationRecordersStatus'
--
-- * 'dcrsrsStatus'
describeConfigurationRecorderStatusResponse
    :: Int -- ^ 'dcrsrsStatus'
    -> DescribeConfigurationRecorderStatusResponse
describeConfigurationRecorderStatusResponse pStatus_ =
    DescribeConfigurationRecorderStatusResponse'
    { _dcrsrsConfigurationRecordersStatus = Nothing
    , _dcrsrsStatus = pStatus_
    }

-- | A list that contains status of the specified recorders.
dcrsrsConfigurationRecordersStatus :: Lens' DescribeConfigurationRecorderStatusResponse [ConfigurationRecorderStatus]
dcrsrsConfigurationRecordersStatus = lens _dcrsrsConfigurationRecordersStatus (\ s a -> s{_dcrsrsConfigurationRecordersStatus = a}) . _Default . _Coerce;

-- | The response status code.
dcrsrsStatus :: Lens' DescribeConfigurationRecorderStatusResponse Int
dcrsrsStatus = lens _dcrsrsStatus (\ s a -> s{_dcrsrsStatus = a});
