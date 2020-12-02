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
-- Module      : Network.AWS.SES.UpdateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an association between a configuration set and a custom domain for open and click event tracking.
--
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Configuring Custom Domains to Handle Open and Click Tracking> in the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide> .
--
module Network.AWS.SES.UpdateConfigurationSetTrackingOptions
    (
    -- * Creating a Request
      updateConfigurationSetTrackingOptions
    , UpdateConfigurationSetTrackingOptions
    -- * Request Lenses
    , ucstoConfigurationSetName
    , ucstoTrackingOptions

    -- * Destructuring the Response
    , updateConfigurationSetTrackingOptionsResponse
    , UpdateConfigurationSetTrackingOptionsResponse
    -- * Response Lenses
    , ucstorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to update the tracking options for a configuration set.
--
--
--
-- /See:/ 'updateConfigurationSetTrackingOptions' smart constructor.
data UpdateConfigurationSetTrackingOptions = UpdateConfigurationSetTrackingOptions'
  { _ucstoConfigurationSetName :: !Text
  , _ucstoTrackingOptions      :: !TrackingOptions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetTrackingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucstoConfigurationSetName' - The name of the configuration set for which you want to update the custom tracking domain.
--
-- * 'ucstoTrackingOptions' - Undocumented member.
updateConfigurationSetTrackingOptions
    :: Text -- ^ 'ucstoConfigurationSetName'
    -> TrackingOptions -- ^ 'ucstoTrackingOptions'
    -> UpdateConfigurationSetTrackingOptions
updateConfigurationSetTrackingOptions pConfigurationSetName_ pTrackingOptions_ =
  UpdateConfigurationSetTrackingOptions'
    { _ucstoConfigurationSetName = pConfigurationSetName_
    , _ucstoTrackingOptions = pTrackingOptions_
    }


-- | The name of the configuration set for which you want to update the custom tracking domain.
ucstoConfigurationSetName :: Lens' UpdateConfigurationSetTrackingOptions Text
ucstoConfigurationSetName = lens _ucstoConfigurationSetName (\ s a -> s{_ucstoConfigurationSetName = a})

-- | Undocumented member.
ucstoTrackingOptions :: Lens' UpdateConfigurationSetTrackingOptions TrackingOptions
ucstoTrackingOptions = lens _ucstoTrackingOptions (\ s a -> s{_ucstoTrackingOptions = a})

instance AWSRequest
           UpdateConfigurationSetTrackingOptions
         where
        type Rs UpdateConfigurationSetTrackingOptions =
             UpdateConfigurationSetTrackingOptionsResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "UpdateConfigurationSetTrackingOptionsResult"
              (\ s h x ->
                 UpdateConfigurationSetTrackingOptionsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           UpdateConfigurationSetTrackingOptions
         where

instance NFData UpdateConfigurationSetTrackingOptions
         where

instance ToHeaders
           UpdateConfigurationSetTrackingOptions
         where
        toHeaders = const mempty

instance ToPath UpdateConfigurationSetTrackingOptions
         where
        toPath = const "/"

instance ToQuery
           UpdateConfigurationSetTrackingOptions
         where
        toQuery UpdateConfigurationSetTrackingOptions'{..}
          = mconcat
              ["Action" =:
                 ("UpdateConfigurationSetTrackingOptions" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _ucstoConfigurationSetName,
               "TrackingOptions" =: _ucstoTrackingOptions]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'updateConfigurationSetTrackingOptionsResponse' smart constructor.
newtype UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse'
  { _ucstorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetTrackingOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucstorsResponseStatus' - -- | The response status code.
updateConfigurationSetTrackingOptionsResponse
    :: Int -- ^ 'ucstorsResponseStatus'
    -> UpdateConfigurationSetTrackingOptionsResponse
updateConfigurationSetTrackingOptionsResponse pResponseStatus_ =
  UpdateConfigurationSetTrackingOptionsResponse'
    {_ucstorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucstorsResponseStatus :: Lens' UpdateConfigurationSetTrackingOptionsResponse Int
ucstorsResponseStatus = lens _ucstorsResponseStatus (\ s a -> s{_ucstorsResponseStatus = a})

instance NFData
           UpdateConfigurationSetTrackingOptionsResponse
         where
