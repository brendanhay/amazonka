{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetOpenIdConnectProvider
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified OpenID Connect provider.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetOpenIdConnectProvider.html AWS API Reference> for GetOpenIdConnectProvider.
module Network.AWS.IAM.GetOpenIdConnectProvider
    (
    -- * Creating a Request
      GetOpenIdConnectProvider
    , getOpenIdConnectProvider
    -- * Request Lenses
    , goicpOpenIdConnectProviderARN

    -- * Destructuring the Response
    , GetOpenIdConnectProviderResponse
    , getOpenIdConnectProviderResponse
    -- * Response Lenses
    , goicprsCreateDate
    , goicprsURL
    , goicprsThumbprintList
    , goicprsClientIdList
    , goicprsStatus
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
getOpenIdConnectProvider pOpenIdConnectProviderARN_ =
    GetOpenIdConnectProvider'
    { _goicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN_
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
        request = postQuery
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
                 ("GetOpenIDConnectProvider" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "OpenIDConnectProviderArn" =:
                 _goicpOpenIdConnectProviderARN]

-- | Contains the response to a successful GetOpenIDConnectProvider request.
--
-- /See:/ 'getOpenIdConnectProviderResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goicprsCreateDate'
--
-- * 'goicprsURL'
--
-- * 'goicprsThumbprintList'
--
-- * 'goicprsClientIdList'
--
-- * 'goicprsStatus'
data GetOpenIdConnectProviderResponse = GetOpenIdConnectProviderResponse'
    { _goicprsCreateDate     :: !(Maybe ISO8601)
    , _goicprsURL            :: !(Maybe Text)
    , _goicprsThumbprintList :: !(Maybe [Text])
    , _goicprsClientIdList   :: !(Maybe [Text])
    , _goicprsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdConnectProviderResponse' smart constructor.
getOpenIdConnectProviderResponse :: Int -> GetOpenIdConnectProviderResponse
getOpenIdConnectProviderResponse pStatus_ =
    GetOpenIdConnectProviderResponse'
    { _goicprsCreateDate = Nothing
    , _goicprsURL = Nothing
    , _goicprsThumbprintList = Nothing
    , _goicprsClientIdList = Nothing
    , _goicprsStatus = pStatus_
    }

-- | The date and time when the IAM OpenID Connect provider entity was
-- created in the AWS account.
goicprsCreateDate :: Lens' GetOpenIdConnectProviderResponse (Maybe UTCTime)
goicprsCreateDate = lens _goicprsCreateDate (\ s a -> s{_goicprsCreateDate = a}) . mapping _Time;

-- | The URL that the IAM OpenID Connect provider is associated with. For
-- more information, see CreateOpenIDConnectProvider.
goicprsURL :: Lens' GetOpenIdConnectProviderResponse (Maybe Text)
goicprsURL = lens _goicprsURL (\ s a -> s{_goicprsURL = a});

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goicprsThumbprintList :: Lens' GetOpenIdConnectProviderResponse [Text]
goicprsThumbprintList = lens _goicprsThumbprintList (\ s a -> s{_goicprsThumbprintList = a}) . _Default . _Coerce;

-- | A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OpenID Connect provider. For more information, see
-- CreateOpenIDConnectProvider.
goicprsClientIdList :: Lens' GetOpenIdConnectProviderResponse [Text]
goicprsClientIdList = lens _goicprsClientIdList (\ s a -> s{_goicprsClientIdList = a}) . _Default . _Coerce;

-- | Undocumented member.
goicprsStatus :: Lens' GetOpenIdConnectProviderResponse Int
goicprsStatus = lens _goicprsStatus (\ s a -> s{_goicprsStatus = a});
