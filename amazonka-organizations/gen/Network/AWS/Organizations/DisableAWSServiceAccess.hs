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
-- Module      : Network.AWS.Organizations.DisableAWSServiceAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the integration of an AWS service (the service that is specified by @ServicePrincipal@ ) with AWS Organizations. When you disable integration, the specified service no longer can create a <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role> in /new/ accounts in your organization. This means the service can't perform operations on your behalf on any new accounts in your organization. The service can still perform operations in older accounts until the service completes its clean-up from AWS Organizations.
--
--
--
--
-- /Important:/ We recommend that you disable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the other service is aware that it can clean up any resources that are required only for the integration. How the service cleans up its resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.
--
-- After you perform the @DisableAWSServiceAccess@ operation, the specified service can no longer perform operations in your organization's accounts unless the operations are explicitly permitted by the IAM policies that are attached to your roles.
--
-- For more information about integrating other services with AWS Organizations, including the list of services that work with Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide/ .
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DisableAWSServiceAccess
    (
    -- * Creating a Request
      disableAWSServiceAccess
    , DisableAWSServiceAccess
    -- * Request Lenses
    , dasaServicePrincipal

    -- * Destructuring the Response
    , disableAWSServiceAccessResponse
    , DisableAWSServiceAccessResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableAWSServiceAccess' smart constructor.
newtype DisableAWSServiceAccess = DisableAWSServiceAccess'
  { _dasaServicePrincipal :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAWSServiceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasaServicePrincipal' - The service principal name of the AWS service for which you want to disable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
disableAWSServiceAccess
    :: Text -- ^ 'dasaServicePrincipal'
    -> DisableAWSServiceAccess
disableAWSServiceAccess pServicePrincipal_ =
  DisableAWSServiceAccess' {_dasaServicePrincipal = pServicePrincipal_}


-- | The service principal name of the AWS service for which you want to disable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
dasaServicePrincipal :: Lens' DisableAWSServiceAccess Text
dasaServicePrincipal = lens _dasaServicePrincipal (\ s a -> s{_dasaServicePrincipal = a})

instance AWSRequest DisableAWSServiceAccess where
        type Rs DisableAWSServiceAccess =
             DisableAWSServiceAccessResponse
        request = postJSON organizations
        response
          = receiveNull DisableAWSServiceAccessResponse'

instance Hashable DisableAWSServiceAccess where

instance NFData DisableAWSServiceAccess where

instance ToHeaders DisableAWSServiceAccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DisableAWSServiceAccess"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableAWSServiceAccess where
        toJSON DisableAWSServiceAccess'{..}
          = object
              (catMaybes
                 [Just ("ServicePrincipal" .= _dasaServicePrincipal)])

instance ToPath DisableAWSServiceAccess where
        toPath = const "/"

instance ToQuery DisableAWSServiceAccess where
        toQuery = const mempty

-- | /See:/ 'disableAWSServiceAccessResponse' smart constructor.
data DisableAWSServiceAccessResponse =
  DisableAWSServiceAccessResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAWSServiceAccessResponse' with the minimum fields required to make a request.
--
disableAWSServiceAccessResponse
    :: DisableAWSServiceAccessResponse
disableAWSServiceAccessResponse = DisableAWSServiceAccessResponse'


instance NFData DisableAWSServiceAccessResponse where
