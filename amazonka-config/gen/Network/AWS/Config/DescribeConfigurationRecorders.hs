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
-- Module      : Network.AWS.Config.DescribeConfigurationRecorders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the specified configuration recorders. If the configuration recorder is not specified, this action returns the details for all configuration recorders associated with the account.
--
--
module Network.AWS.Config.DescribeConfigurationRecorders
    (
    -- * Creating a Request
      describeConfigurationRecorders
    , DescribeConfigurationRecorders
    -- * Request Lenses
    , dcrConfigurationRecorderNames

    -- * Destructuring the Response
    , describeConfigurationRecordersResponse
    , DescribeConfigurationRecordersResponse
    -- * Response Lenses
    , drsConfigurationRecorders
    , drsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DescribeConfigurationRecorders' action.
--
--
--
-- /See:/ 'describeConfigurationRecorders' smart constructor.
newtype DescribeConfigurationRecorders = DescribeConfigurationRecorders'
  { _dcrConfigurationRecorderNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationRecorders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrConfigurationRecorderNames' - A list of configuration recorder names.
describeConfigurationRecorders
    :: DescribeConfigurationRecorders
describeConfigurationRecorders =
  DescribeConfigurationRecorders' {_dcrConfigurationRecorderNames = Nothing}


-- | A list of configuration recorder names.
dcrConfigurationRecorderNames :: Lens' DescribeConfigurationRecorders [Text]
dcrConfigurationRecorderNames = lens _dcrConfigurationRecorderNames (\ s a -> s{_dcrConfigurationRecorderNames = a}) . _Default . _Coerce

instance AWSRequest DescribeConfigurationRecorders
         where
        type Rs DescribeConfigurationRecorders =
             DescribeConfigurationRecordersResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationRecordersResponse' <$>
                   (x .?> "ConfigurationRecorders" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeConfigurationRecorders
         where

instance NFData DescribeConfigurationRecorders where

instance ToHeaders DescribeConfigurationRecorders
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigurationRecorders"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigurationRecorders where
        toJSON DescribeConfigurationRecorders'{..}
          = object
              (catMaybes
                 [("ConfigurationRecorderNames" .=) <$>
                    _dcrConfigurationRecorderNames])

instance ToPath DescribeConfigurationRecorders where
        toPath = const "/"

instance ToQuery DescribeConfigurationRecorders where
        toQuery = const mempty

-- | The output for the 'DescribeConfigurationRecorders' action.
--
--
--
-- /See:/ 'describeConfigurationRecordersResponse' smart constructor.
data DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse'
  { _drsConfigurationRecorders :: !(Maybe [ConfigurationRecorder])
  , _drsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationRecordersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsConfigurationRecorders' - A list that contains the descriptions of the specified configuration recorders.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeConfigurationRecordersResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeConfigurationRecordersResponse
describeConfigurationRecordersResponse pResponseStatus_ =
  DescribeConfigurationRecordersResponse'
    { _drsConfigurationRecorders = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | A list that contains the descriptions of the specified configuration recorders.
drsConfigurationRecorders :: Lens' DescribeConfigurationRecordersResponse [ConfigurationRecorder]
drsConfigurationRecorders = lens _drsConfigurationRecorders (\ s a -> s{_drsConfigurationRecorders = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeConfigurationRecordersResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData
           DescribeConfigurationRecordersResponse
         where
