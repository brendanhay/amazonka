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
-- Module      : Network.AWS.Discovery.UpdateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata about an application.
--
--
module Network.AWS.Discovery.UpdateApplication
    (
    -- * Creating a Request
      updateApplication
    , UpdateApplication
    -- * Request Lenses
    , uaName
    , uaDescription
    , uaConfigurationId

    -- * Destructuring the Response
    , updateApplicationResponse
    , UpdateApplicationResponse
    -- * Response Lenses
    , uarsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { _uaName            :: !(Maybe Text)
  , _uaDescription     :: !(Maybe Text)
  , _uaConfigurationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaName' - New name of the application to be updated.
--
-- * 'uaDescription' - New description of the application to be updated.
--
-- * 'uaConfigurationId' - Configuration ID of the application to be updated.
updateApplication
    :: Text -- ^ 'uaConfigurationId'
    -> UpdateApplication
updateApplication pConfigurationId_ =
  UpdateApplication'
    { _uaName = Nothing
    , _uaDescription = Nothing
    , _uaConfigurationId = pConfigurationId_
    }


-- | New name of the application to be updated.
uaName :: Lens' UpdateApplication (Maybe Text)
uaName = lens _uaName (\ s a -> s{_uaName = a})

-- | New description of the application to be updated.
uaDescription :: Lens' UpdateApplication (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | Configuration ID of the application to be updated.
uaConfigurationId :: Lens' UpdateApplication Text
uaConfigurationId = lens _uaConfigurationId (\ s a -> s{_uaConfigurationId = a})

instance AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        request = postJSON discovery
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateApplicationResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateApplication where

instance NFData UpdateApplication where

instance ToHeaders UpdateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.UpdateApplication"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplication where
        toJSON UpdateApplication'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _uaName,
                  ("description" .=) <$> _uaDescription,
                  Just ("configurationId" .= _uaConfigurationId)])

instance ToPath UpdateApplication where
        toPath = const "/"

instance ToQuery UpdateApplication where
        toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
  { _uarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateApplicationResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateApplicationResponse
updateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse' {_uarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateApplicationResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateApplicationResponse where
