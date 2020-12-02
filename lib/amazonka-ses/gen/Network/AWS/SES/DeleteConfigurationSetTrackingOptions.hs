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
-- Module      : Network.AWS.SES.DeleteConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an association between a configuration set and a custom domain for open and click event tracking.
--
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Configuring Custom Domains to Handle Open and Click Tracking> in the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide> .
--
module Network.AWS.SES.DeleteConfigurationSetTrackingOptions
    (
    -- * Creating a Request
      deleteConfigurationSetTrackingOptions
    , DeleteConfigurationSetTrackingOptions
    -- * Request Lenses
    , dcstoConfigurationSetName

    -- * Destructuring the Response
    , deleteConfigurationSetTrackingOptionsResponse
    , DeleteConfigurationSetTrackingOptionsResponse
    -- * Response Lenses
    , dcstorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete open and click tracking options in a configuration set.
--
--
--
-- /See:/ 'deleteConfigurationSetTrackingOptions' smart constructor.
newtype DeleteConfigurationSetTrackingOptions = DeleteConfigurationSetTrackingOptions'
  { _dcstoConfigurationSetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationSetTrackingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcstoConfigurationSetName' - The name of the configuration set from which you want to delete the tracking options.
deleteConfigurationSetTrackingOptions
    :: Text -- ^ 'dcstoConfigurationSetName'
    -> DeleteConfigurationSetTrackingOptions
deleteConfigurationSetTrackingOptions pConfigurationSetName_ =
  DeleteConfigurationSetTrackingOptions'
    {_dcstoConfigurationSetName = pConfigurationSetName_}


-- | The name of the configuration set from which you want to delete the tracking options.
dcstoConfigurationSetName :: Lens' DeleteConfigurationSetTrackingOptions Text
dcstoConfigurationSetName = lens _dcstoConfigurationSetName (\ s a -> s{_dcstoConfigurationSetName = a})

instance AWSRequest
           DeleteConfigurationSetTrackingOptions
         where
        type Rs DeleteConfigurationSetTrackingOptions =
             DeleteConfigurationSetTrackingOptionsResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "DeleteConfigurationSetTrackingOptionsResult"
              (\ s h x ->
                 DeleteConfigurationSetTrackingOptionsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           DeleteConfigurationSetTrackingOptions
         where

instance NFData DeleteConfigurationSetTrackingOptions
         where

instance ToHeaders
           DeleteConfigurationSetTrackingOptions
         where
        toHeaders = const mempty

instance ToPath DeleteConfigurationSetTrackingOptions
         where
        toPath = const "/"

instance ToQuery
           DeleteConfigurationSetTrackingOptions
         where
        toQuery DeleteConfigurationSetTrackingOptions'{..}
          = mconcat
              ["Action" =:
                 ("DeleteConfigurationSetTrackingOptions" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _dcstoConfigurationSetName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'deleteConfigurationSetTrackingOptionsResponse' smart constructor.
newtype DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse'
  { _dcstorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationSetTrackingOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcstorsResponseStatus' - -- | The response status code.
deleteConfigurationSetTrackingOptionsResponse
    :: Int -- ^ 'dcstorsResponseStatus'
    -> DeleteConfigurationSetTrackingOptionsResponse
deleteConfigurationSetTrackingOptionsResponse pResponseStatus_ =
  DeleteConfigurationSetTrackingOptionsResponse'
    {_dcstorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcstorsResponseStatus :: Lens' DeleteConfigurationSetTrackingOptionsResponse Int
dcstorsResponseStatus = lens _dcstorsResponseStatus (\ s a -> s{_dcstorsResponseStatus = a})

instance NFData
           DeleteConfigurationSetTrackingOptionsResponse
         where
