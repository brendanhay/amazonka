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
-- Module      : Network.AWS.APIGateway.GetSDK
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetSDK
    (
    -- * Creating a Request
      getSDK
    , GetSDK
    -- * Request Lenses
    , gsdkParameters
    , gsdkRestAPIId
    , gsdkStageName
    , gsdkSdkType

    -- * Destructuring the Response
    , getSDKResponse
    , GetSDKResponse
    -- * Response Lenses
    , gsdkrsBody
    , gsdkrsContentDisposition
    , gsdkrsContentType
    , gsdkrsResponseStatus
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getSDK' smart constructor.
data GetSDK = GetSDK'
    { _gsdkParameters :: !(Maybe (Map Text Text))
    , _gsdkRestAPIId  :: !Text
    , _gsdkStageName  :: !Text
    , _gsdkSdkType    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSDK' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdkParameters'
--
-- * 'gsdkRestAPIId'
--
-- * 'gsdkStageName'
--
-- * 'gsdkSdkType'
getSDK
    :: Text -- ^ 'gsdkRestAPIId'
    -> Text -- ^ 'gsdkStageName'
    -> Text -- ^ 'gsdkSdkType'
    -> GetSDK
getSDK pRestAPIId_ pStageName_ pSdkType_ =
    GetSDK'
    { _gsdkParameters = Nothing
    , _gsdkRestAPIId = pRestAPIId_
    , _gsdkStageName = pStageName_
    , _gsdkSdkType = pSdkType_
    }

-- | Undocumented member.
gsdkParameters :: Lens' GetSDK (HashMap Text Text)
gsdkParameters = lens _gsdkParameters (\ s a -> s{_gsdkParameters = a}) . _Default . _Map;

-- | Undocumented member.
gsdkRestAPIId :: Lens' GetSDK Text
gsdkRestAPIId = lens _gsdkRestAPIId (\ s a -> s{_gsdkRestAPIId = a});

-- | Undocumented member.
gsdkStageName :: Lens' GetSDK Text
gsdkStageName = lens _gsdkStageName (\ s a -> s{_gsdkStageName = a});

-- | Undocumented member.
gsdkSdkType :: Lens' GetSDK Text
gsdkSdkType = lens _gsdkSdkType (\ s a -> s{_gsdkSdkType = a});

instance AWSRequest GetSDK where
        type Rs GetSDK = GetSDKResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetSDKResponse' <$>
                   (pure (Just x)) <*> (h .#? "Content-Disposition") <*>
                     (h .#? "Content-Type")
                     <*> (pure (fromEnum s)))

instance Hashable GetSDK

instance ToHeaders GetSDK where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetSDK where
        toPath GetSDK'{..}
          = mconcat
              ["/restapis/", toBS _gsdkRestAPIId, "/stages/",
               toBS _gsdkStageName, "/sdks/", toBS _gsdkSdkType]

instance ToQuery GetSDK where
        toQuery GetSDK'{..}
          = mconcat
              ["parameters" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _gsdkParameters)]

-- | /See:/ 'getSDKResponse' smart constructor.
data GetSDKResponse = GetSDKResponse'
    { _gsdkrsBody               :: !(Maybe (HashMap Text Value))
    , _gsdkrsContentDisposition :: !(Maybe Text)
    , _gsdkrsContentType        :: !(Maybe Text)
    , _gsdkrsResponseStatus     :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSDKResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdkrsBody'
--
-- * 'gsdkrsContentDisposition'
--
-- * 'gsdkrsContentType'
--
-- * 'gsdkrsResponseStatus'
getSDKResponse
    :: Int -- ^ 'gsdkrsResponseStatus'
    -> GetSDKResponse
getSDKResponse pResponseStatus_ =
    GetSDKResponse'
    { _gsdkrsBody = Nothing
    , _gsdkrsContentDisposition = Nothing
    , _gsdkrsContentType = Nothing
    , _gsdkrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gsdkrsBody :: Lens' GetSDKResponse (Maybe (HashMap Text Value))
gsdkrsBody = lens _gsdkrsBody (\ s a -> s{_gsdkrsBody = a});

-- | Undocumented member.
gsdkrsContentDisposition :: Lens' GetSDKResponse (Maybe Text)
gsdkrsContentDisposition = lens _gsdkrsContentDisposition (\ s a -> s{_gsdkrsContentDisposition = a});

-- | Undocumented member.
gsdkrsContentType :: Lens' GetSDKResponse (Maybe Text)
gsdkrsContentType = lens _gsdkrsContentType (\ s a -> s{_gsdkrsContentType = a});

-- | The response status code.
gsdkrsResponseStatus :: Lens' GetSDKResponse Int
gsdkrsResponseStatus = lens _gsdkrsResponseStatus (\ s a -> s{_gsdkrsResponseStatus = a});
