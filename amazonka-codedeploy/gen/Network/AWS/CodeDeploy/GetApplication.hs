{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.GetApplication
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

-- | Gets information about an application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetApplication.html>
module Network.AWS.CodeDeploy.GetApplication
    (
    -- * Request
      GetApplication
    -- ** Request constructor
    , getApplication
    -- ** Request lenses
    , gaApplicationName

    -- * Response
    , GetApplicationResponse
    -- ** Response constructor
    , getApplicationResponse
    -- ** Response lenses
    , garApplication
    , garStatusCode
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get application operation.
--
-- /See:/ 'getApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaApplicationName'
newtype GetApplication = GetApplication'{_gaApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'GetApplication' smart constructor.
getApplication :: Text -> GetApplication
getApplication pApplicationName = GetApplication'{_gaApplicationName = pApplicationName};

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
gaApplicationName :: Lens' GetApplication Text
gaApplicationName = lens _gaApplicationName (\ s a -> s{_gaApplicationName = a});

instance AWSRequest GetApplication where
        type Sv GetApplication = CodeDeploy
        type Rs GetApplication = GetApplicationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetApplicationResponse' <$>
                   (x .?> "application") <*> (pure (fromEnum s)))

instance ToHeaders GetApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetApplication" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetApplication where
        toJSON GetApplication'{..}
          = object ["applicationName" .= _gaApplicationName]

instance ToPath GetApplication where
        toPath = const "/"

instance ToQuery GetApplication where
        toQuery = const mempty

-- | Represents the output of a get application operation.
--
-- /See:/ 'getApplicationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garApplication'
--
-- * 'garStatusCode'
data GetApplicationResponse = GetApplicationResponse'{_garApplication :: Maybe ApplicationInfo, _garStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetApplicationResponse' smart constructor.
getApplicationResponse :: Int -> GetApplicationResponse
getApplicationResponse pStatusCode = GetApplicationResponse'{_garApplication = Nothing, _garStatusCode = pStatusCode};

-- | Information about the application.
garApplication :: Lens' GetApplicationResponse (Maybe ApplicationInfo)
garApplication = lens _garApplication (\ s a -> s{_garApplication = a});

-- | FIXME: Undocumented member.
garStatusCode :: Lens' GetApplicationResponse Int
garStatusCode = lens _garStatusCode (\ s a -> s{_garStatusCode = a});
