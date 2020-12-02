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
-- Module      : Network.AWS.MQ.DescribeConfigurationRevision
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified configuration revision for the specified configuration.
module Network.AWS.MQ.DescribeConfigurationRevision
    (
    -- * Creating a Request
      describeConfigurationRevision
    , DescribeConfigurationRevision
    -- * Request Lenses
    , dcrConfigurationRevision
    , dcrConfigurationId

    -- * Destructuring the Response
    , describeConfigurationRevisionResponse
    , DescribeConfigurationRevisionResponse
    -- * Response Lenses
    , dcrrsConfigurationId
    , dcrrsData
    , dcrrsDescription
    , dcrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConfigurationRevision' smart constructor.
data DescribeConfigurationRevision = DescribeConfigurationRevision'
  { _dcrConfigurationRevision :: !Text
  , _dcrConfigurationId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrConfigurationRevision' - The revision of the configuration.
--
-- * 'dcrConfigurationId' - The unique ID that Amazon MQ generates for the configuration.
describeConfigurationRevision
    :: Text -- ^ 'dcrConfigurationRevision'
    -> Text -- ^ 'dcrConfigurationId'
    -> DescribeConfigurationRevision
describeConfigurationRevision pConfigurationRevision_ pConfigurationId_ =
  DescribeConfigurationRevision'
    { _dcrConfigurationRevision = pConfigurationRevision_
    , _dcrConfigurationId = pConfigurationId_
    }


-- | The revision of the configuration.
dcrConfigurationRevision :: Lens' DescribeConfigurationRevision Text
dcrConfigurationRevision = lens _dcrConfigurationRevision (\ s a -> s{_dcrConfigurationRevision = a})

-- | The unique ID that Amazon MQ generates for the configuration.
dcrConfigurationId :: Lens' DescribeConfigurationRevision Text
dcrConfigurationId = lens _dcrConfigurationId (\ s a -> s{_dcrConfigurationId = a})

instance AWSRequest DescribeConfigurationRevision
         where
        type Rs DescribeConfigurationRevision =
             DescribeConfigurationRevisionResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationRevisionResponse' <$>
                   (x .?> "configurationId") <*> (x .?> "data") <*>
                     (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigurationRevision where

instance NFData DescribeConfigurationRevision where

instance ToHeaders DescribeConfigurationRevision
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeConfigurationRevision where
        toPath DescribeConfigurationRevision'{..}
          = mconcat
              ["/v1/configurations/", toBS _dcrConfigurationId,
               "/revisions/", toBS _dcrConfigurationRevision]

instance ToQuery DescribeConfigurationRevision where
        toQuery = const mempty

-- | /See:/ 'describeConfigurationRevisionResponse' smart constructor.
data DescribeConfigurationRevisionResponse = DescribeConfigurationRevisionResponse'
  { _dcrrsConfigurationId :: !(Maybe Text)
  , _dcrrsData            :: !(Maybe Text)
  , _dcrrsDescription     :: !(Maybe Text)
  , _dcrrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationRevisionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrrsConfigurationId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'dcrrsData' - Required. The base64-encoded XML configuration.
--
-- * 'dcrrsDescription' - The description of the configuration.
--
-- * 'dcrrsResponseStatus' - -- | The response status code.
describeConfigurationRevisionResponse
    :: Int -- ^ 'dcrrsResponseStatus'
    -> DescribeConfigurationRevisionResponse
describeConfigurationRevisionResponse pResponseStatus_ =
  DescribeConfigurationRevisionResponse'
    { _dcrrsConfigurationId = Nothing
    , _dcrrsData = Nothing
    , _dcrrsDescription = Nothing
    , _dcrrsResponseStatus = pResponseStatus_
    }


-- | Required. The unique ID that Amazon MQ generates for the configuration.
dcrrsConfigurationId :: Lens' DescribeConfigurationRevisionResponse (Maybe Text)
dcrrsConfigurationId = lens _dcrrsConfigurationId (\ s a -> s{_dcrrsConfigurationId = a})

-- | Required. The base64-encoded XML configuration.
dcrrsData :: Lens' DescribeConfigurationRevisionResponse (Maybe Text)
dcrrsData = lens _dcrrsData (\ s a -> s{_dcrrsData = a})

-- | The description of the configuration.
dcrrsDescription :: Lens' DescribeConfigurationRevisionResponse (Maybe Text)
dcrrsDescription = lens _dcrrsDescription (\ s a -> s{_dcrrsDescription = a})

-- | -- | The response status code.
dcrrsResponseStatus :: Lens' DescribeConfigurationRevisionResponse Int
dcrrsResponseStatus = lens _dcrrsResponseStatus (\ s a -> s{_dcrrsResponseStatus = a})

instance NFData DescribeConfigurationRevisionResponse
         where
