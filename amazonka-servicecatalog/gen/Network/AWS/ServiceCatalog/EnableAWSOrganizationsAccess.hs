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
-- Module      : Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable portfolio sharing feature through AWS Organizations. This API will allow Service Catalog to receive updates on your organization in order to sync your shares with the current structure. This API can only be called by the master account in the organization.
--
--
-- By calling this API Service Catalog will make a call to organizations:EnableAWSServiceAccess on your behalf so that your shares can be in sync with any changes in your AWS Organizations structure.
--
module Network.AWS.ServiceCatalog.EnableAWSOrganizationsAccess
    (
    -- * Creating a Request
      enableAWSOrganizationsAccess
    , EnableAWSOrganizationsAccess

    -- * Destructuring the Response
    , enableAWSOrganizationsAccessResponse
    , EnableAWSOrganizationsAccessResponse
    -- * Response Lenses
    , eaoarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'enableAWSOrganizationsAccess' smart constructor.
data EnableAWSOrganizationsAccess =
  EnableAWSOrganizationsAccess'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAWSOrganizationsAccess' with the minimum fields required to make a request.
--
enableAWSOrganizationsAccess
    :: EnableAWSOrganizationsAccess
enableAWSOrganizationsAccess = EnableAWSOrganizationsAccess'


instance AWSRequest EnableAWSOrganizationsAccess
         where
        type Rs EnableAWSOrganizationsAccess =
             EnableAWSOrganizationsAccessResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 EnableAWSOrganizationsAccessResponse' <$>
                   (pure (fromEnum s)))

instance Hashable EnableAWSOrganizationsAccess where

instance NFData EnableAWSOrganizationsAccess where

instance ToHeaders EnableAWSOrganizationsAccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.EnableAWSOrganizationsAccess"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableAWSOrganizationsAccess where
        toJSON = const (Object mempty)

instance ToPath EnableAWSOrganizationsAccess where
        toPath = const "/"

instance ToQuery EnableAWSOrganizationsAccess where
        toQuery = const mempty

-- | /See:/ 'enableAWSOrganizationsAccessResponse' smart constructor.
newtype EnableAWSOrganizationsAccessResponse = EnableAWSOrganizationsAccessResponse'
  { _eaoarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAWSOrganizationsAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaoarsResponseStatus' - -- | The response status code.
enableAWSOrganizationsAccessResponse
    :: Int -- ^ 'eaoarsResponseStatus'
    -> EnableAWSOrganizationsAccessResponse
enableAWSOrganizationsAccessResponse pResponseStatus_ =
  EnableAWSOrganizationsAccessResponse'
    {_eaoarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
eaoarsResponseStatus :: Lens' EnableAWSOrganizationsAccessResponse Int
eaoarsResponseStatus = lens _eaoarsResponseStatus (\ s a -> s{_eaoarsResponseStatus = a})

instance NFData EnableAWSOrganizationsAccessResponse
         where
