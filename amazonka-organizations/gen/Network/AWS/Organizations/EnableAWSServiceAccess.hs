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
-- Module      : Network.AWS.Organizations.EnableAWSServiceAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the integration of an AWS service (the service that is specified by @ServicePrincipal@ ) with AWS Organizations. When you enable integration, you allow the specified service to create a <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role> in all the accounts in your organization. This allows the service to perform operations on your behalf in your organization and its accounts.
--
--
-- /Important:/ We recommend that you enable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the service is aware that it can create the resources that are required for the integration. How the service creates those resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.
--
-- For more information about enabling services to integrate with AWS Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide/ .
--
-- This operation can be called only from the organization's master account and only if the organization has <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html enabled all features> .
--
module Network.AWS.Organizations.EnableAWSServiceAccess
    (
    -- * Creating a Request
      enableAWSServiceAccess
    , EnableAWSServiceAccess
    -- * Request Lenses
    , easaServicePrincipal

    -- * Destructuring the Response
    , enableAWSServiceAccessResponse
    , EnableAWSServiceAccessResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableAWSServiceAccess' smart constructor.
newtype EnableAWSServiceAccess = EnableAWSServiceAccess'
  { _easaServicePrincipal :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAWSServiceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'easaServicePrincipal' - The service principal name of the AWS service for which you want to enable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
enableAWSServiceAccess
    :: Text -- ^ 'easaServicePrincipal'
    -> EnableAWSServiceAccess
enableAWSServiceAccess pServicePrincipal_ =
  EnableAWSServiceAccess' {_easaServicePrincipal = pServicePrincipal_}


-- | The service principal name of the AWS service for which you want to enable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
easaServicePrincipal :: Lens' EnableAWSServiceAccess Text
easaServicePrincipal = lens _easaServicePrincipal (\ s a -> s{_easaServicePrincipal = a})

instance AWSRequest EnableAWSServiceAccess where
        type Rs EnableAWSServiceAccess =
             EnableAWSServiceAccessResponse
        request = postJSON organizations
        response
          = receiveNull EnableAWSServiceAccessResponse'

instance Hashable EnableAWSServiceAccess where

instance NFData EnableAWSServiceAccess where

instance ToHeaders EnableAWSServiceAccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.EnableAWSServiceAccess"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableAWSServiceAccess where
        toJSON EnableAWSServiceAccess'{..}
          = object
              (catMaybes
                 [Just ("ServicePrincipal" .= _easaServicePrincipal)])

instance ToPath EnableAWSServiceAccess where
        toPath = const "/"

instance ToQuery EnableAWSServiceAccess where
        toQuery = const mempty

-- | /See:/ 'enableAWSServiceAccessResponse' smart constructor.
data EnableAWSServiceAccessResponse =
  EnableAWSServiceAccessResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAWSServiceAccessResponse' with the minimum fields required to make a request.
--
enableAWSServiceAccessResponse
    :: EnableAWSServiceAccessResponse
enableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'


instance NFData EnableAWSServiceAccessResponse where
