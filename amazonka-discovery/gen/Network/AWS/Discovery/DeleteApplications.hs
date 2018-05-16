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
-- Module      : Network.AWS.Discovery.DeleteApplications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of applications and their associations with configuration items.
--
--
module Network.AWS.Discovery.DeleteApplications
    (
    -- * Creating a Request
      deleteApplications
    , DeleteApplications
    -- * Request Lenses
    , daConfigurationIds

    -- * Destructuring the Response
    , deleteApplicationsResponse
    , DeleteApplicationsResponse
    -- * Response Lenses
    , darsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApplications' smart constructor.
newtype DeleteApplications = DeleteApplications'
  { _daConfigurationIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daConfigurationIds' - Configuration ID of an application to be deleted.
deleteApplications
    :: DeleteApplications
deleteApplications = DeleteApplications' {_daConfigurationIds = mempty}


-- | Configuration ID of an application to be deleted.
daConfigurationIds :: Lens' DeleteApplications [Text]
daConfigurationIds = lens _daConfigurationIds (\ s a -> s{_daConfigurationIds = a}) . _Coerce

instance AWSRequest DeleteApplications where
        type Rs DeleteApplications =
             DeleteApplicationsResponse
        request = postJSON discovery
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteApplicationsResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteApplications where

instance NFData DeleteApplications where

instance ToHeaders DeleteApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DeleteApplications"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApplications where
        toJSON DeleteApplications'{..}
          = object
              (catMaybes
                 [Just ("configurationIds" .= _daConfigurationIds)])

instance ToPath DeleteApplications where
        toPath = const "/"

instance ToQuery DeleteApplications where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationsResponse' smart constructor.
newtype DeleteApplicationsResponse = DeleteApplicationsResponse'
  { _darsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteApplicationsResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DeleteApplicationsResponse
deleteApplicationsResponse pResponseStatus_ =
  DeleteApplicationsResponse' {_darsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteApplicationsResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DeleteApplicationsResponse where
