{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Updates the specified application to have the specified properties.
--
-- If a property (for example, @description@) is not provided, the value
-- remains unchanged. To clear these properties, specify an empty string.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateApplication.html>
module Network.AWS.ElasticBeanstalk.UpdateApplication
    (
    -- * Request
      UpdateApplication
    -- ** Request constructor
    , updateApplication
    -- ** Request lenses
    , uaDescription
    , uaApplicationName

    -- * Response
    , ApplicationDescriptionMessage
    -- ** Response constructor
    , applicationDescriptionMessage
    -- ** Response lenses
    , admApplication
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'updateApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaDescription'
--
-- * 'uaApplicationName'
data UpdateApplication = UpdateApplication'{_uaDescription :: Maybe Text, _uaApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateApplication' smart constructor.
updateApplication :: Text -> UpdateApplication
updateApplication pApplicationName = UpdateApplication'{_uaDescription = Nothing, _uaApplicationName = pApplicationName};

-- | A new description for the application.
--
-- Default: If not specified, AWS Elastic Beanstalk does not update the
-- description.
uaDescription :: Lens' UpdateApplication (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a});

-- | The name of the application to update. If no such application is found,
-- @UpdateApplication@ returns an @InvalidParameterValue@ error.
uaApplicationName :: Lens' UpdateApplication Text
uaApplicationName = lens _uaApplicationName (\ s a -> s{_uaApplicationName = a});

instance AWSRequest UpdateApplication where
        type Sv UpdateApplication = ElasticBeanstalk
        type Rs UpdateApplication =
             ApplicationDescriptionMessage
        request = post
        response
          = receiveXMLWrapper "UpdateApplicationResult"
              (\ s h x -> parseXML x)

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
