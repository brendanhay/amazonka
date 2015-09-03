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
-- Module      : Network.AWS.CodeDeploy.CreateApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new application.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateApplication.html AWS API Reference> for CreateApplication.
module Network.AWS.CodeDeploy.CreateApplication
    (
    -- * Creating a Request
      createApplication
    , CreateApplication
    -- * Request Lenses
    , caApplicationName

    -- * Destructuring the Response
    , createApplicationResponse
    , CreateApplicationResponse
    -- * Response Lenses
    , carsApplicationId
    , carsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create application operation.
--
-- /See:/ 'createApplication' smart constructor.
newtype CreateApplication = CreateApplication'
    { _caApplicationName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caApplicationName'
createApplication
    :: Text -- ^ 'caApplicationName'
    -> CreateApplication
createApplication pApplicationName_ =
    CreateApplication'
    { _caApplicationName = pApplicationName_
    }

-- | The name of the application. This name must be unique with the
-- applicable IAM user or AWS account.
caApplicationName :: Lens' CreateApplication Text
caApplicationName = lens _caApplicationName (\ s a -> s{_caApplicationName = a});

instance AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' <$>
                   (x .?> "applicationId") <*> (pure (fromEnum s)))

instance ToHeaders CreateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.CreateApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApplication where
        toJSON CreateApplication'{..}
          = object
              (catMaybes
                 [Just ("applicationName" .= _caApplicationName)])

instance ToPath CreateApplication where
        toPath = const "/"

instance ToQuery CreateApplication where
        toQuery = const mempty

-- | Represents the output of a create application operation.
--
-- /See:/ 'createApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
    { _carsApplicationId  :: !(Maybe Text)
    , _carsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsApplicationId'
--
-- * 'carsResponseStatus'
createApplicationResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateApplicationResponse
createApplicationResponse pResponseStatus_ =
    CreateApplicationResponse'
    { _carsApplicationId = Nothing
    , _carsResponseStatus = pResponseStatus_
    }

-- | A unique application ID.
carsApplicationId :: Lens' CreateApplicationResponse (Maybe Text)
carsApplicationId = lens _carsApplicationId (\ s a -> s{_carsApplicationId = a});

-- | The response status code.
carsResponseStatus :: Lens' CreateApplicationResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a});
