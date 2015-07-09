{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetOpenIdConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified OpenID Connect provider.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetOpenIdConnectProvider.html>
module Network.AWS.IAM.GetOpenIdConnectProvider
    (
    -- * Request
      GetOpenIdConnectProvider
    -- ** Request constructor
    , getOpenIdConnectProvider
    -- ** Request lenses
    , goicpOpenIdConnectProviderARN

    -- * Response
    , GetOpenIdConnectProviderResponse
    -- ** Response constructor
    , getOpenIdConnectProviderResponse
    -- ** Response lenses
    , goicprCreateDate
    , goicprURL
    , goicprThumbprintList
    , goicprClientIdList
    , goicprStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getOpenIdConnectProvider' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goicpOpenIdConnectProviderARN'
newtype GetOpenIdConnectProvider = GetOpenIdConnectProvider'
    { _goicpOpenIdConnectProviderARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdConnectProvider' smart constructor.
getOpenIdConnectProvider :: Text -> GetOpenIdConnectProvider
getOpenIdConnectProvider pOpenIdConnectProviderARN =
    GetOpenIdConnectProvider'
    { _goicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN
    }

-- | The Amazon Resource Name (ARN) of the IAM OpenID Connect (OIDC) provider
-- to get information for. You can get a list of OIDC provider ARNs by
-- using the ListOpenIDConnectProviders action.
goicpOpenIdConnectProviderARN :: Lens' GetOpenIdConnectProvider Text
goicpOpenIdConnectProviderARN = lens _goicpOpenIdConnectProviderARN (\ s a -> s{_goicpOpenIdConnectProviderARN = a});

instance AWSRequest GetOpenIdConnectProvider where
        type Sv GetOpenIdConnectProvider = IAM
        type Rs GetOpenIdConnectProvider =
             GetOpenIdConnectProviderResponse
        request = post
        response
          = receiveXMLWrapper "GetOpenIDConnectProviderResult"
              (\ s h x ->
                 GetOpenIdConnectProviderResponse' <$>
                   (x .@? "CreateDate") <*> (x .@? "Url") <*>
                     (x .@? "ThumbprintList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "ClientIDList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders GetOpenIdConnectProvider where
        toHeaders = const mempty

instance ToPath GetOpenIdConnectProvider where
        toPath = const "/"

instance ToQuery GetOpenIdConnectProvider where
        toQuery GetOpenIdConnectProvider'{..}
          = mconcat
              ["Action" =:
                 ("GetOpenIdConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _goicpOpenIdConnectProviderARN]

-- | Contains the response to a successful GetOpenIDConnectProvider request.
--
-- /See:/ 'getOpenIdConnectProviderResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goicprCreateDate'
--
-- * 'goicprURL'
--
-- * 'goicprThumbprintList'
--
-- * 'goicprClientIdList'
--
-- * 'goicprStatus'
data GetOpenIdConnectProviderResponse = GetOpenIdConnectProviderResponse'
    { _goicprCreateDate     :: !(Maybe ISO8601)
    , _goicprURL            :: !(Maybe Text)
    , _goicprThumbprintList :: !(Maybe [Text])
    , _goicprClientIdList   :: !(Maybe [Text])
    , _goicprStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdConnectProviderResponse' smart constructor.
getOpenIdConnectProviderResponse :: Int -> GetOpenIdConnectProviderResponse
getOpenIdConnectProviderResponse pStatus =
    GetOpenIdConnectProviderResponse'
    { _goicprCreateDate = Nothing
    , _goicprURL = Nothing
    , _goicprThumbprintList = Nothing
    , _goicprClientIdList = Nothing
    , _goicprStatus = pStatus
    }

-- | The date and time when the IAM OpenID Connect provider entity was
-- created in the AWS account.
goicprCreateDate :: Lens' GetOpenIdConnectProviderResponse (Maybe UTCTime)
goicprCreateDate = lens _goicprCreateDate (\ s a -> s{_goicprCreateDate = a}) . mapping _Time;

-- | The URL that the IAM OpenID Connect provider is associated with. For
-- more information, see CreateOpenIDConnectProvider.
goicprURL :: Lens' GetOpenIdConnectProviderResponse (Maybe Text)
goicprURL = lens _goicprURL (\ s a -> s{_goicprURL = a});

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goicprThumbprintList :: Lens' GetOpenIdConnectProviderResponse [Text]
goicprThumbprintList = lens _goicprThumbprintList (\ s a -> s{_goicprThumbprintList = a}) . _Default;

-- | A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goicprClientIdList :: Lens' GetOpenIdConnectProviderResponse [Text]
goicprClientIdList = lens _goicprClientIdList (\ s a -> s{_goicprClientIdList = a}) . _Default;

-- | FIXME: Undocumented member.
goicprStatus :: Lens' GetOpenIdConnectProviderResponse Int
goicprStatus = lens _goicprStatus (\ s a -> s{_goicprStatus = a});
