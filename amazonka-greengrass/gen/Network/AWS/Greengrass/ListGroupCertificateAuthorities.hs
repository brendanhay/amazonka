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
-- Module      : Network.AWS.Greengrass.ListGroupCertificateAuthorities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current CAs for a group.
module Network.AWS.Greengrass.ListGroupCertificateAuthorities
    (
    -- * Creating a Request
      listGroupCertificateAuthorities
    , ListGroupCertificateAuthorities
    -- * Request Lenses
    , lgcaGroupId

    -- * Destructuring the Response
    , listGroupCertificateAuthoritiesResponse
    , ListGroupCertificateAuthoritiesResponse
    -- * Response Lenses
    , lgcarsGroupCertificateAuthorities
    , lgcarsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGroupCertificateAuthorities' smart constructor.
newtype ListGroupCertificateAuthorities = ListGroupCertificateAuthorities'
  { _lgcaGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupCertificateAuthorities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgcaGroupId' - The ID of the AWS Greengrass group.
listGroupCertificateAuthorities
    :: Text -- ^ 'lgcaGroupId'
    -> ListGroupCertificateAuthorities
listGroupCertificateAuthorities pGroupId_ =
  ListGroupCertificateAuthorities' {_lgcaGroupId = pGroupId_}


-- | The ID of the AWS Greengrass group.
lgcaGroupId :: Lens' ListGroupCertificateAuthorities Text
lgcaGroupId = lens _lgcaGroupId (\ s a -> s{_lgcaGroupId = a})

instance AWSRequest ListGroupCertificateAuthorities
         where
        type Rs ListGroupCertificateAuthorities =
             ListGroupCertificateAuthoritiesResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupCertificateAuthoritiesResponse' <$>
                   (x .?> "GroupCertificateAuthorities" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListGroupCertificateAuthorities
         where

instance NFData ListGroupCertificateAuthorities where

instance ToHeaders ListGroupCertificateAuthorities
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListGroupCertificateAuthorities where
        toPath ListGroupCertificateAuthorities'{..}
          = mconcat
              ["/greengrass/groups/", toBS _lgcaGroupId,
               "/certificateauthorities"]

instance ToQuery ListGroupCertificateAuthorities
         where
        toQuery = const mempty

-- | /See:/ 'listGroupCertificateAuthoritiesResponse' smart constructor.
data ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse'
  { _lgcarsGroupCertificateAuthorities :: !(Maybe [GroupCertificateAuthorityProperties])
  , _lgcarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupCertificateAuthoritiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgcarsGroupCertificateAuthorities' - A list of certificate authorities associated with the group.
--
-- * 'lgcarsResponseStatus' - -- | The response status code.
listGroupCertificateAuthoritiesResponse
    :: Int -- ^ 'lgcarsResponseStatus'
    -> ListGroupCertificateAuthoritiesResponse
listGroupCertificateAuthoritiesResponse pResponseStatus_ =
  ListGroupCertificateAuthoritiesResponse'
    { _lgcarsGroupCertificateAuthorities = Nothing
    , _lgcarsResponseStatus = pResponseStatus_
    }


-- | A list of certificate authorities associated with the group.
lgcarsGroupCertificateAuthorities :: Lens' ListGroupCertificateAuthoritiesResponse [GroupCertificateAuthorityProperties]
lgcarsGroupCertificateAuthorities = lens _lgcarsGroupCertificateAuthorities (\ s a -> s{_lgcarsGroupCertificateAuthorities = a}) . _Default . _Coerce

-- | -- | The response status code.
lgcarsResponseStatus :: Lens' ListGroupCertificateAuthoritiesResponse Int
lgcarsResponseStatus = lens _lgcarsResponseStatus (\ s a -> s{_lgcarsResponseStatus = a})

instance NFData
           ListGroupCertificateAuthoritiesResponse
         where
