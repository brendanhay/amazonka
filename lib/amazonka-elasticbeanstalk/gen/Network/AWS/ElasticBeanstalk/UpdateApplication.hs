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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application to have the specified properties.
--
--
module Network.AWS.ElasticBeanstalk.UpdateApplication
    (
    -- * Creating a Request
      updateApplication
    , UpdateApplication
    -- * Request Lenses
    , uaDescription
    , uaApplicationName

    -- * Destructuring the Response
    , applicationDescriptionMessage
    , ApplicationDescriptionMessage
    -- * Response Lenses
    , admApplication
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an application.
--
--
--
-- /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { _uaDescription     :: !(Maybe Text)
  , _uaApplicationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaDescription' - A new description for the application. Default: If not specified, AWS Elastic Beanstalk does not update the description.
--
-- * 'uaApplicationName' - The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error.
updateApplication
    :: Text -- ^ 'uaApplicationName'
    -> UpdateApplication
updateApplication pApplicationName_ =
  UpdateApplication'
    {_uaDescription = Nothing, _uaApplicationName = pApplicationName_}


-- | A new description for the application. Default: If not specified, AWS Elastic Beanstalk does not update the description.
uaDescription :: Lens' UpdateApplication (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | The name of the application to update. If no such application is found, @UpdateApplication@ returns an @InvalidParameterValue@ error.
uaApplicationName :: Lens' UpdateApplication Text
uaApplicationName = lens _uaApplicationName (\ s a -> s{_uaApplicationName = a})

instance AWSRequest UpdateApplication where
        type Rs UpdateApplication =
             ApplicationDescriptionMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "UpdateApplicationResult"
              (\ s h x -> parseXML x)

instance Hashable UpdateApplication where

instance NFData UpdateApplication where

instance ToHeaders UpdateApplication where
        toHeaders = const mempty

instance ToPath UpdateApplication where
        toPath = const "/"

instance ToQuery UpdateApplication where
        toQuery UpdateApplication'{..}
          = mconcat
              ["Action" =: ("UpdateApplication" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Description" =: _uaDescription,
               "ApplicationName" =: _uaApplicationName]
