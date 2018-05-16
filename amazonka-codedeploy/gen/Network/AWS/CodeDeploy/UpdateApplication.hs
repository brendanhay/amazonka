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
-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the name of an application.
--
--
module Network.AWS.CodeDeploy.UpdateApplication
    (
    -- * Creating a Request
      updateApplication
    , UpdateApplication
    -- * Request Lenses
    , uaNewApplicationName
    , uaApplicationName

    -- * Destructuring the Response
    , updateApplicationResponse
    , UpdateApplicationResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an UpdateApplication operation.
--
--
--
-- /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { _uaNewApplicationName :: !(Maybe Text)
  , _uaApplicationName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaNewApplicationName' - The new name to give the application.
--
-- * 'uaApplicationName' - The current name of the application you want to change.
updateApplication
    :: UpdateApplication
updateApplication =
  UpdateApplication'
    {_uaNewApplicationName = Nothing, _uaApplicationName = Nothing}


-- | The new name to give the application.
uaNewApplicationName :: Lens' UpdateApplication (Maybe Text)
uaNewApplicationName = lens _uaNewApplicationName (\ s a -> s{_uaNewApplicationName = a})

-- | The current name of the application you want to change.
uaApplicationName :: Lens' UpdateApplication (Maybe Text)
uaApplicationName = lens _uaApplicationName (\ s a -> s{_uaApplicationName = a})

instance AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        request = postJSON codeDeploy
        response = receiveNull UpdateApplicationResponse'

instance Hashable UpdateApplication where

instance NFData UpdateApplication where

instance ToHeaders UpdateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.UpdateApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplication where
        toJSON UpdateApplication'{..}
          = object
              (catMaybes
                 [("newApplicationName" .=) <$> _uaNewApplicationName,
                  ("applicationName" .=) <$> _uaApplicationName])

instance ToPath UpdateApplication where
        toPath = const "/"

instance ToQuery UpdateApplication where
        toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
data UpdateApplicationResponse =
  UpdateApplicationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
updateApplicationResponse
    :: UpdateApplicationResponse
updateApplicationResponse = UpdateApplicationResponse'


instance NFData UpdateApplicationResponse where
