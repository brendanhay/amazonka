{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.GetApplicationRevision
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

-- | Gets information about an application revision.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetApplicationRevision.html>
module Network.AWS.CodeDeploy.GetApplicationRevision
    (
    -- * Request
      GetApplicationRevision
    -- ** Request constructor
    , getApplicationRevision
    -- ** Request lenses
    , garApplicationName
    , garRevision

    -- * Response
    , GetApplicationRevisionResponse
    -- ** Response constructor
    , getApplicationRevisionResponse
    -- ** Response lenses
    , garrRevisionInfo
    , garrRevision
    , garrApplicationName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'getApplicationRevision' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garApplicationName'
--
-- * 'garRevision'
data GetApplicationRevision = GetApplicationRevision'{_garApplicationName :: Text, _garRevision :: RevisionLocation} deriving (Eq, Read, Show)

-- | 'GetApplicationRevision' smart constructor.
getApplicationRevision :: Text -> RevisionLocation -> GetApplicationRevision
getApplicationRevision pApplicationName pRevision = GetApplicationRevision'{_garApplicationName = pApplicationName, _garRevision = pRevision};

-- | The name of the application that corresponds to the revision.
garApplicationName :: Lens' GetApplicationRevision Text
garApplicationName = lens _garApplicationName (\ s a -> s{_garApplicationName = a});

-- | Information about the application revision to get, including the
-- revision\'s type and its location.
garRevision :: Lens' GetApplicationRevision RevisionLocation
garRevision = lens _garRevision (\ s a -> s{_garRevision = a});

instance AWSRequest GetApplicationRevision where
        type Sv GetApplicationRevision = CodeDeploy
        type Rs GetApplicationRevision =
             GetApplicationRevisionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetApplicationRevisionResponse' <$>
                   x .?> "revisionInfo" <*> x .?> "revision" <*>
                     x .:> "applicationName")

instance ToHeaders GetApplicationRevision where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetApplicationRevision" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetApplicationRevision where
        toJSON GetApplicationRevision'{..}
          = object
              ["applicationName" .= _garApplicationName,
               "revision" .= _garRevision]

instance ToPath GetApplicationRevision where
        toPath = const "/"

instance ToQuery GetApplicationRevision where
        toQuery = const mempty

-- | /See:/ 'getApplicationRevisionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garrRevisionInfo'
--
-- * 'garrRevision'
--
-- * 'garrApplicationName'
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'{_garrRevisionInfo :: Maybe GenericRevisionInfo, _garrRevision :: Maybe RevisionLocation, _garrApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'GetApplicationRevisionResponse' smart constructor.
getApplicationRevisionResponse :: Text -> GetApplicationRevisionResponse
getApplicationRevisionResponse pApplicationName = GetApplicationRevisionResponse'{_garrRevisionInfo = Nothing, _garrRevision = Nothing, _garrApplicationName = pApplicationName};

-- | General information about the revision.
garrRevisionInfo :: Lens' GetApplicationRevisionResponse (Maybe GenericRevisionInfo)
garrRevisionInfo = lens _garrRevisionInfo (\ s a -> s{_garrRevisionInfo = a});

-- | Additional information about the revision, including the revision\'s
-- type and its location.
garrRevision :: Lens' GetApplicationRevisionResponse (Maybe RevisionLocation)
garrRevision = lens _garrRevision (\ s a -> s{_garrRevision = a});

-- | The name of the application that corresponds to the revision.
garrApplicationName :: Lens' GetApplicationRevisionResponse Text
garrApplicationName = lens _garrApplicationName (\ s a -> s{_garrApplicationName = a});
