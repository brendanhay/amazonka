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
-- Module      : Network.AWS.Organizations.ListAWSServiceAccessForOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the AWS services that you enabled to integrate with your organization. After a service on this list creates the resources that it requires for the integration, it can perform operations on your organization and its accounts.
--
--
-- For more information about integrating other services with AWS Organizations, including the list of services that currently work with Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide/ .
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListAWSServiceAccessForOrganization
    (
    -- * Creating a Request
      listAWSServiceAccessForOrganization
    , ListAWSServiceAccessForOrganization
    -- * Request Lenses
    , lasafoNextToken
    , lasafoMaxResults

    -- * Destructuring the Response
    , listAWSServiceAccessForOrganizationResponse
    , ListAWSServiceAccessForOrganizationResponse
    -- * Response Lenses
    , lasaforsNextToken
    , lasaforsEnabledServicePrincipals
    , lasaforsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAWSServiceAccessForOrganization' smart constructor.
data ListAWSServiceAccessForOrganization = ListAWSServiceAccessForOrganization'
  { _lasafoNextToken  :: !(Maybe Text)
  , _lasafoMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAWSServiceAccessForOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasafoNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lasafoMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listAWSServiceAccessForOrganization
    :: ListAWSServiceAccessForOrganization
listAWSServiceAccessForOrganization =
  ListAWSServiceAccessForOrganization'
    {_lasafoNextToken = Nothing, _lasafoMaxResults = Nothing}


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lasafoNextToken :: Lens' ListAWSServiceAccessForOrganization (Maybe Text)
lasafoNextToken = lens _lasafoNextToken (\ s a -> s{_lasafoNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lasafoMaxResults :: Lens' ListAWSServiceAccessForOrganization (Maybe Natural)
lasafoMaxResults = lens _lasafoMaxResults (\ s a -> s{_lasafoMaxResults = a}) . mapping _Nat

instance AWSPager ListAWSServiceAccessForOrganization
         where
        page rq rs
          | stop (rs ^. lasaforsNextToken) = Nothing
          | stop (rs ^. lasaforsEnabledServicePrincipals) =
            Nothing
          | otherwise =
            Just $ rq &
              lasafoNextToken .~ rs ^. lasaforsNextToken

instance AWSRequest
           ListAWSServiceAccessForOrganization
         where
        type Rs ListAWSServiceAccessForOrganization =
             ListAWSServiceAccessForOrganizationResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListAWSServiceAccessForOrganizationResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "EnabledServicePrincipals" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListAWSServiceAccessForOrganization
         where

instance NFData ListAWSServiceAccessForOrganization
         where

instance ToHeaders
           ListAWSServiceAccessForOrganization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListAWSServiceAccessForOrganization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAWSServiceAccessForOrganization
         where
        toJSON ListAWSServiceAccessForOrganization'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lasafoNextToken,
                  ("MaxResults" .=) <$> _lasafoMaxResults])

instance ToPath ListAWSServiceAccessForOrganization
         where
        toPath = const "/"

instance ToQuery ListAWSServiceAccessForOrganization
         where
        toQuery = const mempty

-- | /See:/ 'listAWSServiceAccessForOrganizationResponse' smart constructor.
data ListAWSServiceAccessForOrganizationResponse = ListAWSServiceAccessForOrganizationResponse'
  { _lasaforsNextToken                :: !(Maybe Text)
  , _lasaforsEnabledServicePrincipals :: !(Maybe [EnabledServicePrincipal])
  , _lasaforsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAWSServiceAccessForOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lasaforsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lasaforsEnabledServicePrincipals' - A list of the service principals for the services that are enabled to integrate with your organization. Each principal is a structure that includes the name and the date that it was enabled for integration with AWS Organizations.
--
-- * 'lasaforsResponseStatus' - -- | The response status code.
listAWSServiceAccessForOrganizationResponse
    :: Int -- ^ 'lasaforsResponseStatus'
    -> ListAWSServiceAccessForOrganizationResponse
listAWSServiceAccessForOrganizationResponse pResponseStatus_ =
  ListAWSServiceAccessForOrganizationResponse'
    { _lasaforsNextToken = Nothing
    , _lasaforsEnabledServicePrincipals = Nothing
    , _lasaforsResponseStatus = pResponseStatus_
    }


-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lasaforsNextToken :: Lens' ListAWSServiceAccessForOrganizationResponse (Maybe Text)
lasaforsNextToken = lens _lasaforsNextToken (\ s a -> s{_lasaforsNextToken = a})

-- | A list of the service principals for the services that are enabled to integrate with your organization. Each principal is a structure that includes the name and the date that it was enabled for integration with AWS Organizations.
lasaforsEnabledServicePrincipals :: Lens' ListAWSServiceAccessForOrganizationResponse [EnabledServicePrincipal]
lasaforsEnabledServicePrincipals = lens _lasaforsEnabledServicePrincipals (\ s a -> s{_lasaforsEnabledServicePrincipals = a}) . _Default . _Coerce

-- | -- | The response status code.
lasaforsResponseStatus :: Lens' ListAWSServiceAccessForOrganizationResponse Int
lasaforsResponseStatus = lens _lasaforsResponseStatus (\ s a -> s{_lasaforsResponseStatus = a})

instance NFData
           ListAWSServiceAccessForOrganizationResponse
         where
