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
-- Module      : Network.AWS.MQ.UpdateConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration.
module Network.AWS.MQ.UpdateConfiguration
    (
    -- * Creating a Request
      updateConfiguration
    , UpdateConfiguration
    -- * Request Lenses
    , ucData
    , ucDescription
    , ucConfigurationId

    -- * Destructuring the Response
    , updateConfigurationResponse
    , UpdateConfigurationResponse
    -- * Response Lenses
    , ucrsARN
    , ucrsLatestRevision
    , ucrsWarnings
    , ucrsName
    , ucrsId
    , ucrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates the specified configuration.
--
-- /See:/ 'updateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { _ucData            :: !(Maybe Text)
  , _ucDescription     :: !(Maybe Text)
  , _ucConfigurationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucData' - Required. The base64-encoded XML configuration.
--
-- * 'ucDescription' - The description of the configuration.
--
-- * 'ucConfigurationId' - The unique ID that Amazon MQ generates for the configuration.
updateConfiguration
    :: Text -- ^ 'ucConfigurationId'
    -> UpdateConfiguration
updateConfiguration pConfigurationId_ =
  UpdateConfiguration'
    { _ucData = Nothing
    , _ucDescription = Nothing
    , _ucConfigurationId = pConfigurationId_
    }


-- | Required. The base64-encoded XML configuration.
ucData :: Lens' UpdateConfiguration (Maybe Text)
ucData = lens _ucData (\ s a -> s{_ucData = a})

-- | The description of the configuration.
ucDescription :: Lens' UpdateConfiguration (Maybe Text)
ucDescription = lens _ucDescription (\ s a -> s{_ucDescription = a})

-- | The unique ID that Amazon MQ generates for the configuration.
ucConfigurationId :: Lens' UpdateConfiguration Text
ucConfigurationId = lens _ucConfigurationId (\ s a -> s{_ucConfigurationId = a})

instance AWSRequest UpdateConfiguration where
        type Rs UpdateConfiguration =
             UpdateConfigurationResponse
        request = putJSON mq
        response
          = receiveJSON
              (\ s h x ->
                 UpdateConfigurationResponse' <$>
                   (x .?> "arn") <*> (x .?> "latestRevision") <*>
                     (x .?> "warnings" .!@ mempty)
                     <*> (x .?> "name")
                     <*> (x .?> "id")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateConfiguration where

instance NFData UpdateConfiguration where

instance ToHeaders UpdateConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateConfiguration where
        toJSON UpdateConfiguration'{..}
          = object
              (catMaybes
                 [("data" .=) <$> _ucData,
                  ("description" .=) <$> _ucDescription])

instance ToPath UpdateConfiguration where
        toPath UpdateConfiguration'{..}
          = mconcat
              ["/v1/configurations/", toBS _ucConfigurationId]

instance ToQuery UpdateConfiguration where
        toQuery = const mempty

-- | /See:/ 'updateConfigurationResponse' smart constructor.
data UpdateConfigurationResponse = UpdateConfigurationResponse'
  { _ucrsARN            :: !(Maybe Text)
  , _ucrsLatestRevision :: !(Maybe ConfigurationRevision)
  , _ucrsWarnings       :: !(Maybe [SanitizationWarning])
  , _ucrsName           :: !(Maybe Text)
  , _ucrsId             :: !(Maybe Text)
  , _ucrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsARN' - Required. The Amazon Resource Name (ARN) of the configuration.
--
-- * 'ucrsLatestRevision' - The latest revision of the configuration.
--
-- * 'ucrsWarnings' - The list of the first 20 warnings about the configuration XML elements or attributes that were sanitized.
--
-- * 'ucrsName' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- * 'ucrsId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateConfigurationResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateConfigurationResponse
updateConfigurationResponse pResponseStatus_ =
  UpdateConfigurationResponse'
    { _ucrsARN = Nothing
    , _ucrsLatestRevision = Nothing
    , _ucrsWarnings = Nothing
    , _ucrsName = Nothing
    , _ucrsId = Nothing
    , _ucrsResponseStatus = pResponseStatus_
    }


-- | Required. The Amazon Resource Name (ARN) of the configuration.
ucrsARN :: Lens' UpdateConfigurationResponse (Maybe Text)
ucrsARN = lens _ucrsARN (\ s a -> s{_ucrsARN = a})

-- | The latest revision of the configuration.
ucrsLatestRevision :: Lens' UpdateConfigurationResponse (Maybe ConfigurationRevision)
ucrsLatestRevision = lens _ucrsLatestRevision (\ s a -> s{_ucrsLatestRevision = a})

-- | The list of the first 20 warnings about the configuration XML elements or attributes that were sanitized.
ucrsWarnings :: Lens' UpdateConfigurationResponse [SanitizationWarning]
ucrsWarnings = lens _ucrsWarnings (\ s a -> s{_ucrsWarnings = a}) . _Default . _Coerce

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
ucrsName :: Lens' UpdateConfigurationResponse (Maybe Text)
ucrsName = lens _ucrsName (\ s a -> s{_ucrsName = a})

-- | Required. The unique ID that Amazon MQ generates for the configuration.
ucrsId :: Lens' UpdateConfigurationResponse (Maybe Text)
ucrsId = lens _ucrsId (\ s a -> s{_ucrsId = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateConfigurationResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateConfigurationResponse where
