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
-- Module      : Network.AWS.CodeDeploy.GetApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetApplication.html AWS API Reference> for GetApplication.
module Network.AWS.CodeDeploy.GetApplication
    (
    -- * Creating a Request
      getApplication
    , GetApplication
    -- * Request Lenses
    , gaApplicationName

    -- * Destructuring the Response
    , getApplicationResponse
    , GetApplicationResponse
    -- * Response Lenses
    , garsApplication
    , garsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get application operation.
--
-- /See:/ 'getApplication' smart constructor.
newtype GetApplication = GetApplication'
    { _gaApplicationName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaApplicationName'
getApplication
    :: Text -- ^ 'gaApplicationName'
    -> GetApplication
getApplication pApplicationName_ =
    GetApplication'
    { _gaApplicationName = pApplicationName_
    }

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
gaApplicationName :: Lens' GetApplication Text
gaApplicationName = lens _gaApplicationName (\ s a -> s{_gaApplicationName = a});

instance AWSRequest GetApplication where
        type Rs GetApplication = GetApplicationResponse
        request = postJSON codeDeploy
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
          = object
              (catMaybes
                 [Just ("applicationName" .= _gaApplicationName)])

instance ToPath GetApplication where
        toPath = const "/"

instance ToQuery GetApplication where
        toQuery = const mempty

-- | Represents the output of a get application operation.
--
-- /See:/ 'getApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
    { _garsApplication    :: !(Maybe ApplicationInfo)
    , _garsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsApplication'
--
-- * 'garsResponseStatus'
getApplicationResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GetApplicationResponse
getApplicationResponse pResponseStatus_ =
    GetApplicationResponse'
    { _garsApplication = Nothing
    , _garsResponseStatus = pResponseStatus_
    }

-- | Information about the application.
garsApplication :: Lens' GetApplicationResponse (Maybe ApplicationInfo)
garsApplication = lens _garsApplication (\ s a -> s{_garsApplication = a});

-- | The response status code.
garsResponseStatus :: Lens' GetApplicationResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a});
