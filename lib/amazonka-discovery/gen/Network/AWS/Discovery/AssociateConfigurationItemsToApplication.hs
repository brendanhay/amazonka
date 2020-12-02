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
-- Module      : Network.AWS.Discovery.AssociateConfigurationItemsToApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more configuration items with an application.
--
--
module Network.AWS.Discovery.AssociateConfigurationItemsToApplication
    (
    -- * Creating a Request
      associateConfigurationItemsToApplication
    , AssociateConfigurationItemsToApplication
    -- * Request Lenses
    , acitaApplicationConfigurationId
    , acitaConfigurationIds

    -- * Destructuring the Response
    , associateConfigurationItemsToApplicationResponse
    , AssociateConfigurationItemsToApplicationResponse
    -- * Response Lenses
    , acitarsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateConfigurationItemsToApplication' smart constructor.
data AssociateConfigurationItemsToApplication = AssociateConfigurationItemsToApplication'
  { _acitaApplicationConfigurationId :: !Text
  , _acitaConfigurationIds           :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateConfigurationItemsToApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acitaApplicationConfigurationId' - The configuration ID of an application with which items are to be associated.
--
-- * 'acitaConfigurationIds' - The ID of each configuration item to be associated with an application.
associateConfigurationItemsToApplication
    :: Text -- ^ 'acitaApplicationConfigurationId'
    -> AssociateConfigurationItemsToApplication
associateConfigurationItemsToApplication pApplicationConfigurationId_ =
  AssociateConfigurationItemsToApplication'
    { _acitaApplicationConfigurationId = pApplicationConfigurationId_
    , _acitaConfigurationIds = mempty
    }


-- | The configuration ID of an application with which items are to be associated.
acitaApplicationConfigurationId :: Lens' AssociateConfigurationItemsToApplication Text
acitaApplicationConfigurationId = lens _acitaApplicationConfigurationId (\ s a -> s{_acitaApplicationConfigurationId = a})

-- | The ID of each configuration item to be associated with an application.
acitaConfigurationIds :: Lens' AssociateConfigurationItemsToApplication [Text]
acitaConfigurationIds = lens _acitaConfigurationIds (\ s a -> s{_acitaConfigurationIds = a}) . _Coerce

instance AWSRequest
           AssociateConfigurationItemsToApplication
         where
        type Rs AssociateConfigurationItemsToApplication =
             AssociateConfigurationItemsToApplicationResponse
        request = postJSON discovery
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateConfigurationItemsToApplicationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           AssociateConfigurationItemsToApplication
         where

instance NFData
           AssociateConfigurationItemsToApplication
         where

instance ToHeaders
           AssociateConfigurationItemsToApplication
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.AssociateConfigurationItemsToApplication"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           AssociateConfigurationItemsToApplication
         where
        toJSON AssociateConfigurationItemsToApplication'{..}
          = object
              (catMaybes
                 [Just
                    ("applicationConfigurationId" .=
                       _acitaApplicationConfigurationId),
                  Just ("configurationIds" .= _acitaConfigurationIds)])

instance ToPath
           AssociateConfigurationItemsToApplication
         where
        toPath = const "/"

instance ToQuery
           AssociateConfigurationItemsToApplication
         where
        toQuery = const mempty

-- | /See:/ 'associateConfigurationItemsToApplicationResponse' smart constructor.
newtype AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse'
  { _acitarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateConfigurationItemsToApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acitarsResponseStatus' - -- | The response status code.
associateConfigurationItemsToApplicationResponse
    :: Int -- ^ 'acitarsResponseStatus'
    -> AssociateConfigurationItemsToApplicationResponse
associateConfigurationItemsToApplicationResponse pResponseStatus_ =
  AssociateConfigurationItemsToApplicationResponse'
    {_acitarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
acitarsResponseStatus :: Lens' AssociateConfigurationItemsToApplicationResponse Int
acitarsResponseStatus = lens _acitarsResponseStatus (\ s a -> s{_acitarsResponseStatus = a})

instance NFData
           AssociateConfigurationItemsToApplicationResponse
         where
