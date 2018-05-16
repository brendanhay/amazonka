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
-- Module      : Network.AWS.CloudSearch.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the access policies that control access to the domain's document and search endpoints. By default, shows the configuration with any pending changes. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DescribeServiceAccessPolicies
    (
    -- * Creating a Request
      describeServiceAccessPolicies
    , DescribeServiceAccessPolicies
    -- * Request Lenses
    , dsapDeployed
    , dsapDomainName

    -- * Destructuring the Response
    , describeServiceAccessPoliciesResponse
    , DescribeServiceAccessPoliciesResponse
    -- * Response Lenses
    , dsaprsResponseStatus
    , dsaprsAccessPolicies
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeServiceAccessPolicies' @ operation. Specifies the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
--
--
-- /See:/ 'describeServiceAccessPolicies' smart constructor.
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'
  { _dsapDeployed   :: !(Maybe Bool)
  , _dsapDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServiceAccessPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsapDeployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- * 'dsapDomainName' - The name of the domain you want to describe.
describeServiceAccessPolicies
    :: Text -- ^ 'dsapDomainName'
    -> DescribeServiceAccessPolicies
describeServiceAccessPolicies pDomainName_ =
  DescribeServiceAccessPolicies'
    {_dsapDeployed = Nothing, _dsapDomainName = pDomainName_}


-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
dsapDeployed :: Lens' DescribeServiceAccessPolicies (Maybe Bool)
dsapDeployed = lens _dsapDeployed (\ s a -> s{_dsapDeployed = a})

-- | The name of the domain you want to describe.
dsapDomainName :: Lens' DescribeServiceAccessPolicies Text
dsapDomainName = lens _dsapDomainName (\ s a -> s{_dsapDomainName = a})

instance AWSRequest DescribeServiceAccessPolicies
         where
        type Rs DescribeServiceAccessPolicies =
             DescribeServiceAccessPoliciesResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper
              "DescribeServiceAccessPoliciesResult"
              (\ s h x ->
                 DescribeServiceAccessPoliciesResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AccessPolicies"))

instance Hashable DescribeServiceAccessPolicies where

instance NFData DescribeServiceAccessPolicies where

instance ToHeaders DescribeServiceAccessPolicies
         where
        toHeaders = const mempty

instance ToPath DescribeServiceAccessPolicies where
        toPath = const "/"

instance ToQuery DescribeServiceAccessPolicies where
        toQuery DescribeServiceAccessPolicies'{..}
          = mconcat
              ["Action" =:
                 ("DescribeServiceAccessPolicies" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _dsapDeployed,
               "DomainName" =: _dsapDomainName]

-- | The result of a @DescribeServiceAccessPolicies@ request.
--
--
--
-- /See:/ 'describeServiceAccessPoliciesResponse' smart constructor.
data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse'
  { _dsaprsResponseStatus :: !Int
  , _dsaprsAccessPolicies :: !AccessPoliciesStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServiceAccessPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaprsResponseStatus' - -- | The response status code.
--
-- * 'dsaprsAccessPolicies' - The access rules configured for the domain specified in the request.
describeServiceAccessPoliciesResponse
    :: Int -- ^ 'dsaprsResponseStatus'
    -> AccessPoliciesStatus -- ^ 'dsaprsAccessPolicies'
    -> DescribeServiceAccessPoliciesResponse
describeServiceAccessPoliciesResponse pResponseStatus_ pAccessPolicies_ =
  DescribeServiceAccessPoliciesResponse'
    { _dsaprsResponseStatus = pResponseStatus_
    , _dsaprsAccessPolicies = pAccessPolicies_
    }


-- | -- | The response status code.
dsaprsResponseStatus :: Lens' DescribeServiceAccessPoliciesResponse Int
dsaprsResponseStatus = lens _dsaprsResponseStatus (\ s a -> s{_dsaprsResponseStatus = a})

-- | The access rules configured for the domain specified in the request.
dsaprsAccessPolicies :: Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprsAccessPolicies = lens _dsaprsAccessPolicies (\ s a -> s{_dsaprsAccessPolicies = a})

instance NFData DescribeServiceAccessPoliciesResponse
         where
