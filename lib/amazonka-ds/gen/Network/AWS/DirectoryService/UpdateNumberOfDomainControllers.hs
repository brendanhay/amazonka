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
-- Module      : Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes domain controllers to or from the directory. Based on the difference between current value and new value (provided through this API call), domain controllers will be added or removed. It may take up to 45 minutes for any new domain controllers to become fully active once the requested number of domain controllers is updated. During this time, you cannot make another update request.
--
--
module Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
    (
    -- * Creating a Request
      updateNumberOfDomainControllers
    , UpdateNumberOfDomainControllers
    -- * Request Lenses
    , unodcDirectoryId
    , unodcDesiredNumber

    -- * Destructuring the Response
    , updateNumberOfDomainControllersResponse
    , UpdateNumberOfDomainControllersResponse
    -- * Response Lenses
    , unodcrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateNumberOfDomainControllers' smart constructor.
data UpdateNumberOfDomainControllers = UpdateNumberOfDomainControllers'
  { _unodcDirectoryId   :: !Text
  , _unodcDesiredNumber :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNumberOfDomainControllers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unodcDirectoryId' - Identifier of the directory to which the domain controllers will be added or removed.
--
-- * 'unodcDesiredNumber' - The number of domain controllers desired in the directory.
updateNumberOfDomainControllers
    :: Text -- ^ 'unodcDirectoryId'
    -> Natural -- ^ 'unodcDesiredNumber'
    -> UpdateNumberOfDomainControllers
updateNumberOfDomainControllers pDirectoryId_ pDesiredNumber_ =
  UpdateNumberOfDomainControllers'
    { _unodcDirectoryId = pDirectoryId_
    , _unodcDesiredNumber = _Nat # pDesiredNumber_
    }


-- | Identifier of the directory to which the domain controllers will be added or removed.
unodcDirectoryId :: Lens' UpdateNumberOfDomainControllers Text
unodcDirectoryId = lens _unodcDirectoryId (\ s a -> s{_unodcDirectoryId = a})

-- | The number of domain controllers desired in the directory.
unodcDesiredNumber :: Lens' UpdateNumberOfDomainControllers Natural
unodcDesiredNumber = lens _unodcDesiredNumber (\ s a -> s{_unodcDesiredNumber = a}) . _Nat

instance AWSRequest UpdateNumberOfDomainControllers
         where
        type Rs UpdateNumberOfDomainControllers =
             UpdateNumberOfDomainControllersResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateNumberOfDomainControllersResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateNumberOfDomainControllers
         where

instance NFData UpdateNumberOfDomainControllers where

instance ToHeaders UpdateNumberOfDomainControllers
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.UpdateNumberOfDomainControllers"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNumberOfDomainControllers where
        toJSON UpdateNumberOfDomainControllers'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _unodcDirectoryId),
                  Just ("DesiredNumber" .= _unodcDesiredNumber)])

instance ToPath UpdateNumberOfDomainControllers where
        toPath = const "/"

instance ToQuery UpdateNumberOfDomainControllers
         where
        toQuery = const mempty

-- | /See:/ 'updateNumberOfDomainControllersResponse' smart constructor.
newtype UpdateNumberOfDomainControllersResponse = UpdateNumberOfDomainControllersResponse'
  { _unodcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNumberOfDomainControllersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unodcrsResponseStatus' - -- | The response status code.
updateNumberOfDomainControllersResponse
    :: Int -- ^ 'unodcrsResponseStatus'
    -> UpdateNumberOfDomainControllersResponse
updateNumberOfDomainControllersResponse pResponseStatus_ =
  UpdateNumberOfDomainControllersResponse'
    {_unodcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
unodcrsResponseStatus :: Lens' UpdateNumberOfDomainControllersResponse Int
unodcrsResponseStatus = lens _unodcrsResponseStatus (\ s a -> s{_unodcrsResponseStatus = a})

instance NFData
           UpdateNumberOfDomainControllersResponse
         where
