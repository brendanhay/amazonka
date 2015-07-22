{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateApplication.html>
module Network.AWS.CodeDeploy.CreateApplication
    (
    -- * Request
      CreateApplication
    -- ** Request constructor
    , createApplication
    -- ** Request lenses
    , carqApplicationName

    -- * Response
    , CreateApplicationResponse
    -- ** Response constructor
    , createApplicationResponse
    -- ** Response lenses
    , carsApplicationId
    , carsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create application operation.
--
-- /See:/ 'createApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carqApplicationName'
newtype CreateApplication = CreateApplication'
    { _carqApplicationName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateApplication' smart constructor.
createApplication :: Text -> CreateApplication
createApplication pApplicationName =
    CreateApplication'
    { _carqApplicationName = pApplicationName
    }

-- | The name of the application. This name must be unique with the
-- applicable IAM user or AWS account.
carqApplicationName :: Lens' CreateApplication Text
carqApplicationName = lens _carqApplicationName (\ s a -> s{_carqApplicationName = a});

instance AWSRequest CreateApplication where
        type Sv CreateApplication = CodeDeploy
        type Rs CreateApplication = CreateApplicationResponse
        request = postJSON
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
          = object ["applicationName" .= _carqApplicationName]

instance ToPath CreateApplication where
        toPath = const "/"

instance ToQuery CreateApplication where
        toQuery = const mempty

-- | Represents the output of a create application operation.
--
-- /See:/ 'createApplicationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carsApplicationId'
--
-- * 'carsStatus'
data CreateApplicationResponse = CreateApplicationResponse'
    { _carsApplicationId :: !(Maybe Text)
    , _carsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateApplicationResponse' smart constructor.
createApplicationResponse :: Int -> CreateApplicationResponse
createApplicationResponse pStatus =
    CreateApplicationResponse'
    { _carsApplicationId = Nothing
    , _carsStatus = pStatus
    }

-- | A unique application ID.
carsApplicationId :: Lens' CreateApplicationResponse (Maybe Text)
carsApplicationId = lens _carsApplicationId (\ s a -> s{_carsApplicationId = a});

-- | FIXME: Undocumented member.
carsStatus :: Lens' CreateApplicationResponse Int
carsStatus = lens _carsStatus (\ s a -> s{_carsStatus = a});
