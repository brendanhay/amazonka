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
-- Module      : Network.AWS.IAM.GetOpenIdConnectProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified OpenID Connect (OIDC) provider resource object in IAM.
--
--
module Network.AWS.IAM.GetOpenIdConnectProvider
    (
    -- * Creating a Request
      getOpenIdConnectProvider
    , GetOpenIdConnectProvider
    -- * Request Lenses
    , goicpOpenIdConnectProviderARN

    -- * Destructuring the Response
    , getOpenIdConnectProviderResponse
    , GetOpenIdConnectProviderResponse
    -- * Response Lenses
    , goicprsCreateDate
    , goicprsURL
    , goicprsThumbprintList
    , goicprsClientIdList
    , goicprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOpenIdConnectProvider' smart constructor.
newtype GetOpenIdConnectProvider = GetOpenIdConnectProvider'
  { _goicpOpenIdConnectProviderARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goicpOpenIdConnectProviderARN' - The Amazon Resource Name (ARN) of the OIDC provider resource object in IAM to get information for. You can get a list of OIDC provider resource ARNs by using the 'ListOpenIDConnectProviders' operation. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
getOpenIdConnectProvider
    :: Text -- ^ 'goicpOpenIdConnectProviderARN'
    -> GetOpenIdConnectProvider
getOpenIdConnectProvider pOpenIdConnectProviderARN_ =
  GetOpenIdConnectProvider'
    {_goicpOpenIdConnectProviderARN = pOpenIdConnectProviderARN_}


-- | The Amazon Resource Name (ARN) of the OIDC provider resource object in IAM to get information for. You can get a list of OIDC provider resource ARNs by using the 'ListOpenIDConnectProviders' operation. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
goicpOpenIdConnectProviderARN :: Lens' GetOpenIdConnectProvider Text
goicpOpenIdConnectProviderARN = lens _goicpOpenIdConnectProviderARN (\ s a -> s{_goicpOpenIdConnectProviderARN = a})

instance AWSRequest GetOpenIdConnectProvider where
        type Rs GetOpenIdConnectProvider =
             GetOpenIdConnectProviderResponse
        request = postQuery iam
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

instance Hashable GetOpenIdConnectProvider where

instance NFData GetOpenIdConnectProvider where

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

-- | Contains the response to a successful 'GetOpenIDConnectProvider' request.
--
--
--
-- /See:/ 'getOpenIdConnectProviderResponse' smart constructor.
data GetOpenIdConnectProviderResponse = GetOpenIdConnectProviderResponse'
  { _goicprsCreateDate     :: !(Maybe ISO8601)
  , _goicprsURL            :: !(Maybe Text)
  , _goicprsThumbprintList :: !(Maybe [Text])
  , _goicprsClientIdList   :: !(Maybe [Text])
  , _goicprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goicprsCreateDate' - The date and time when the IAM OIDC provider resource object was created in the AWS account.
--
-- * 'goicprsURL' - The URL that the IAM OIDC provider resource object is associated with. For more information, see 'CreateOpenIDConnectProvider' .
--
-- * 'goicprsThumbprintList' - A list of certificate thumbprints that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
--
-- * 'goicprsClientIdList' - A list of client IDs (also known as audiences) that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
--
-- * 'goicprsResponseStatus' - -- | The response status code.
getOpenIdConnectProviderResponse
    :: Int -- ^ 'goicprsResponseStatus'
    -> GetOpenIdConnectProviderResponse
getOpenIdConnectProviderResponse pResponseStatus_ =
  GetOpenIdConnectProviderResponse'
    { _goicprsCreateDate = Nothing
    , _goicprsURL = Nothing
    , _goicprsThumbprintList = Nothing
    , _goicprsClientIdList = Nothing
    , _goicprsResponseStatus = pResponseStatus_
    }


-- | The date and time when the IAM OIDC provider resource object was created in the AWS account.
goicprsCreateDate :: Lens' GetOpenIdConnectProviderResponse (Maybe UTCTime)
goicprsCreateDate = lens _goicprsCreateDate (\ s a -> s{_goicprsCreateDate = a}) . mapping _Time

-- | The URL that the IAM OIDC provider resource object is associated with. For more information, see 'CreateOpenIDConnectProvider' .
goicprsURL :: Lens' GetOpenIdConnectProviderResponse (Maybe Text)
goicprsURL = lens _goicprsURL (\ s a -> s{_goicprsURL = a})

-- | A list of certificate thumbprints that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
goicprsThumbprintList :: Lens' GetOpenIdConnectProviderResponse [Text]
goicprsThumbprintList = lens _goicprsThumbprintList (\ s a -> s{_goicprsThumbprintList = a}) . _Default . _Coerce

-- | A list of client IDs (also known as audiences) that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
goicprsClientIdList :: Lens' GetOpenIdConnectProviderResponse [Text]
goicprsClientIdList = lens _goicprsClientIdList (\ s a -> s{_goicprsClientIdList = a}) . _Default . _Coerce

-- | -- | The response status code.
goicprsResponseStatus :: Lens' GetOpenIdConnectProviderResponse Int
goicprsResponseStatus = lens _goicprsResponseStatus (\ s a -> s{_goicprsResponseStatus = a})

instance NFData GetOpenIdConnectProviderResponse
         where
