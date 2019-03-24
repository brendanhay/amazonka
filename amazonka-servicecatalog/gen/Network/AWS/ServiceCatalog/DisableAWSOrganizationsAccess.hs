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
-- Module      : Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable portfolio sharing through AWS Organizations feature. This feature will not delete your current shares but it will prevent you from creating new shares throughout your organization. Current shares will not be in sync with your organization structure if it changes after calling this API. This API can only be called by the master account in the organization.
--
--
module Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
    (
    -- * Creating a Request
      disableAWSOrganizationsAccess
    , DisableAWSOrganizationsAccess

    -- * Destructuring the Response
    , disableAWSOrganizationsAccessResponse
    , DisableAWSOrganizationsAccessResponse
    -- * Response Lenses
    , daoarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'disableAWSOrganizationsAccess' smart constructor.
data DisableAWSOrganizationsAccess =
  DisableAWSOrganizationsAccess'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAWSOrganizationsAccess' with the minimum fields required to make a request.
--
disableAWSOrganizationsAccess
    :: DisableAWSOrganizationsAccess
disableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'


instance AWSRequest DisableAWSOrganizationsAccess
         where
        type Rs DisableAWSOrganizationsAccess =
             DisableAWSOrganizationsAccessResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DisableAWSOrganizationsAccessResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisableAWSOrganizationsAccess where

instance NFData DisableAWSOrganizationsAccess where

instance ToHeaders DisableAWSOrganizationsAccess
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DisableAWSOrganizationsAccess"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableAWSOrganizationsAccess where
        toJSON = const (Object mempty)

instance ToPath DisableAWSOrganizationsAccess where
        toPath = const "/"

instance ToQuery DisableAWSOrganizationsAccess where
        toQuery = const mempty

-- | /See:/ 'disableAWSOrganizationsAccessResponse' smart constructor.
newtype DisableAWSOrganizationsAccessResponse = DisableAWSOrganizationsAccessResponse'
  { _daoarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAWSOrganizationsAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daoarsResponseStatus' - -- | The response status code.
disableAWSOrganizationsAccessResponse
    :: Int -- ^ 'daoarsResponseStatus'
    -> DisableAWSOrganizationsAccessResponse
disableAWSOrganizationsAccessResponse pResponseStatus_ =
  DisableAWSOrganizationsAccessResponse'
    {_daoarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
daoarsResponseStatus :: Lens' DisableAWSOrganizationsAccessResponse Int
daoarsResponseStatus = lens _daoarsResponseStatus (\ s a -> s{_daoarsResponseStatus = a})

instance NFData DisableAWSOrganizationsAccessResponse
         where
