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
-- Module      : Network.AWS.SES.DescribeConfigurationSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the specified configuration set. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DescribeConfigurationSet
    (
    -- * Creating a Request
      describeConfigurationSet
    , DescribeConfigurationSet
    -- * Request Lenses
    , dcsConfigurationSetAttributeNames
    , dcsConfigurationSetName

    -- * Destructuring the Response
    , describeConfigurationSetResponse
    , DescribeConfigurationSetResponse
    -- * Response Lenses
    , dcsrsTrackingOptions
    , dcsrsConfigurationSet
    , dcsrsReputationOptions
    , dcsrsEventDestinations
    , dcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to return the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'describeConfigurationSet' smart constructor.
data DescribeConfigurationSet = DescribeConfigurationSet'
  { _dcsConfigurationSetAttributeNames :: !(Maybe [ConfigurationSetAttribute])
  , _dcsConfigurationSetName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsConfigurationSetAttributeNames' - A list of configuration set attributes to return.
--
-- * 'dcsConfigurationSetName' - The name of the configuration set to describe.
describeConfigurationSet
    :: Text -- ^ 'dcsConfigurationSetName'
    -> DescribeConfigurationSet
describeConfigurationSet pConfigurationSetName_ =
  DescribeConfigurationSet'
    { _dcsConfigurationSetAttributeNames = Nothing
    , _dcsConfigurationSetName = pConfigurationSetName_
    }


-- | A list of configuration set attributes to return.
dcsConfigurationSetAttributeNames :: Lens' DescribeConfigurationSet [ConfigurationSetAttribute]
dcsConfigurationSetAttributeNames = lens _dcsConfigurationSetAttributeNames (\ s a -> s{_dcsConfigurationSetAttributeNames = a}) . _Default . _Coerce

-- | The name of the configuration set to describe.
dcsConfigurationSetName :: Lens' DescribeConfigurationSet Text
dcsConfigurationSetName = lens _dcsConfigurationSetName (\ s a -> s{_dcsConfigurationSetName = a})

instance AWSRequest DescribeConfigurationSet where
        type Rs DescribeConfigurationSet =
             DescribeConfigurationSetResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "DescribeConfigurationSetResult"
              (\ s h x ->
                 DescribeConfigurationSetResponse' <$>
                   (x .@? "TrackingOptions") <*>
                     (x .@? "ConfigurationSet")
                     <*> (x .@? "ReputationOptions")
                     <*>
                     (x .@? "EventDestinations" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigurationSet where

instance NFData DescribeConfigurationSet where

instance ToHeaders DescribeConfigurationSet where
        toHeaders = const mempty

instance ToPath DescribeConfigurationSet where
        toPath = const "/"

instance ToQuery DescribeConfigurationSet where
        toQuery DescribeConfigurationSet'{..}
          = mconcat
              ["Action" =:
                 ("DescribeConfigurationSet" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetAttributeNames" =:
                 toQuery
                   (toQueryList "member" <$>
                      _dcsConfigurationSetAttributeNames),
               "ConfigurationSetName" =: _dcsConfigurationSetName]

-- | Represents the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'describeConfigurationSetResponse' smart constructor.
data DescribeConfigurationSetResponse = DescribeConfigurationSetResponse'
  { _dcsrsTrackingOptions   :: !(Maybe TrackingOptions)
  , _dcsrsConfigurationSet  :: !(Maybe ConfigurationSet)
  , _dcsrsReputationOptions :: !(Maybe ReputationOptions)
  , _dcsrsEventDestinations :: !(Maybe [EventDestination])
  , _dcsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsrsTrackingOptions' - The name of the custom open and click tracking domain associated with the configuration set.
--
-- * 'dcsrsConfigurationSet' - The configuration set object associated with the specified configuration set.
--
-- * 'dcsrsReputationOptions' - An object that represents the reputation settings for the configuration set.
--
-- * 'dcsrsEventDestinations' - A list of event destinations associated with the configuration set.
--
-- * 'dcsrsResponseStatus' - -- | The response status code.
describeConfigurationSetResponse
    :: Int -- ^ 'dcsrsResponseStatus'
    -> DescribeConfigurationSetResponse
describeConfigurationSetResponse pResponseStatus_ =
  DescribeConfigurationSetResponse'
    { _dcsrsTrackingOptions = Nothing
    , _dcsrsConfigurationSet = Nothing
    , _dcsrsReputationOptions = Nothing
    , _dcsrsEventDestinations = Nothing
    , _dcsrsResponseStatus = pResponseStatus_
    }


-- | The name of the custom open and click tracking domain associated with the configuration set.
dcsrsTrackingOptions :: Lens' DescribeConfigurationSetResponse (Maybe TrackingOptions)
dcsrsTrackingOptions = lens _dcsrsTrackingOptions (\ s a -> s{_dcsrsTrackingOptions = a})

-- | The configuration set object associated with the specified configuration set.
dcsrsConfigurationSet :: Lens' DescribeConfigurationSetResponse (Maybe ConfigurationSet)
dcsrsConfigurationSet = lens _dcsrsConfigurationSet (\ s a -> s{_dcsrsConfigurationSet = a})

-- | An object that represents the reputation settings for the configuration set.
dcsrsReputationOptions :: Lens' DescribeConfigurationSetResponse (Maybe ReputationOptions)
dcsrsReputationOptions = lens _dcsrsReputationOptions (\ s a -> s{_dcsrsReputationOptions = a})

-- | A list of event destinations associated with the configuration set.
dcsrsEventDestinations :: Lens' DescribeConfigurationSetResponse [EventDestination]
dcsrsEventDestinations = lens _dcsrsEventDestinations (\ s a -> s{_dcsrsEventDestinations = a}) . _Default . _Coerce

-- | -- | The response status code.
dcsrsResponseStatus :: Lens' DescribeConfigurationSetResponse Int
dcsrsResponseStatus = lens _dcsrsResponseStatus (\ s a -> s{_dcsrsResponseStatus = a})

instance NFData DescribeConfigurationSetResponse
         where
