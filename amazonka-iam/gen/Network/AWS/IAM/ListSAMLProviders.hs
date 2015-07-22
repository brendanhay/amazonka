{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the SAML providers in the account.
--
-- This operation requires
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSAMLProviders.html>
module Network.AWS.IAM.ListSAMLProviders
    (
    -- * Request
      ListSAMLProviders
    -- ** Request constructor
    , listSAMLProviders

    -- * Response
    , ListSAMLProvidersResponse
    -- ** Response constructor
    , listSAMLProvidersResponse
    -- ** Response lenses
    , lsamlprsSAMLProviderList
    , lsamlprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listSAMLProviders' smart constructor.
data ListSAMLProviders =
    ListSAMLProviders'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSAMLProviders' smart constructor.
listSAMLProviders :: ListSAMLProviders
listSAMLProviders = ListSAMLProviders'

instance AWSRequest ListSAMLProviders where
        type Sv ListSAMLProviders = IAM
        type Rs ListSAMLProviders = ListSAMLProvidersResponse
        request = post
        response
          = receiveXMLWrapper "ListSAMLProvidersResult"
              (\ s h x ->
                 ListSAMLProvidersResponse' <$>
                   (x .@? "SAMLProviderList" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListSAMLProviders where
        toHeaders = const mempty

instance ToPath ListSAMLProviders where
        toPath = const "/"

instance ToQuery ListSAMLProviders where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("ListSAMLProviders" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful ListSAMLProviders request.
--
-- /See:/ 'listSAMLProvidersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsamlprsSAMLProviderList'
--
-- * 'lsamlprsStatus'
data ListSAMLProvidersResponse = ListSAMLProvidersResponse'
    { _lsamlprsSAMLProviderList :: !(Maybe [SAMLProviderListEntry])
    , _lsamlprsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSAMLProvidersResponse' smart constructor.
listSAMLProvidersResponse :: Int -> ListSAMLProvidersResponse
listSAMLProvidersResponse pStatus =
    ListSAMLProvidersResponse'
    { _lsamlprsSAMLProviderList = Nothing
    , _lsamlprsStatus = pStatus
    }

-- | The list of SAML providers for this account.
lsamlprsSAMLProviderList :: Lens' ListSAMLProvidersResponse [SAMLProviderListEntry]
lsamlprsSAMLProviderList = lens _lsamlprsSAMLProviderList (\ s a -> s{_lsamlprsSAMLProviderList = a}) . _Default;

-- | FIXME: Undocumented member.
lsamlprsStatus :: Lens' ListSAMLProvidersResponse Int
lsamlprsStatus = lens _lsamlprsStatus (\ s a -> s{_lsamlprsStatus = a});
