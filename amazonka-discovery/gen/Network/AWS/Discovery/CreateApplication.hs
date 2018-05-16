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
-- Module      : Network.AWS.Discovery.CreateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application with the given name and description.
--
--
module Network.AWS.Discovery.CreateApplication
    (
    -- * Creating a Request
      createApplication
    , CreateApplication
    -- * Request Lenses
    , caDescription
    , caName

    -- * Destructuring the Response
    , createApplicationResponse
    , CreateApplicationResponse
    -- * Response Lenses
    , carsConfigurationId
    , carsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createApplication' smart constructor.
data CreateApplication = CreateApplication'
  { _caDescription :: !(Maybe Text)
  , _caName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDescription' - Description of the application to be created.
--
-- * 'caName' - Name of the application to be created.
createApplication
    :: Text -- ^ 'caName'
    -> CreateApplication
createApplication pName_ =
  CreateApplication' {_caDescription = Nothing, _caName = pName_}


-- | Description of the application to be created.
caDescription :: Lens' CreateApplication (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | Name of the application to be created.
caName :: Lens' CreateApplication Text
caName = lens _caName (\ s a -> s{_caName = a})

instance AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' <$>
                   (x .?> "configurationId") <*> (pure (fromEnum s)))

instance Hashable CreateApplication where

instance NFData CreateApplication where

instance ToHeaders CreateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.CreateApplication"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApplication where
        toJSON CreateApplication'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _caDescription,
                  Just ("name" .= _caName)])

instance ToPath CreateApplication where
        toPath = const "/"

instance ToQuery CreateApplication where
        toQuery = const mempty

-- | /See:/ 'createApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { _carsConfigurationId :: !(Maybe Text)
  , _carsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsConfigurationId' - Configuration ID of an application to be created.
--
-- * 'carsResponseStatus' - -- | The response status code.
createApplicationResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateApplicationResponse
createApplicationResponse pResponseStatus_ =
  CreateApplicationResponse'
    {_carsConfigurationId = Nothing, _carsResponseStatus = pResponseStatus_}


-- | Configuration ID of an application to be created.
carsConfigurationId :: Lens' CreateApplicationResponse (Maybe Text)
carsConfigurationId = lens _carsConfigurationId (\ s a -> s{_carsConfigurationId = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateApplicationResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateApplicationResponse where
