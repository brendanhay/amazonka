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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a client SDK for a 'RestApi' and 'Stage' .
--
--
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

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request a new generated client SDK for a 'RestApi' and 'Stage' .
--
--
--
-- /See:/ 'getSDK' smart constructor.
data GetSDK = GetSDK'
  { _gsdkParameters :: !(Maybe (Map Text Text))
  , _gsdkRestAPIId  :: !Text
  , _gsdkStageName  :: !Text
  , _gsdkSdkType    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSDK' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdkParameters' - A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required.
--
-- * 'gsdkRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gsdkStageName' - [Required] The name of the 'Stage' that the SDK will use.
--
-- * 'gsdkSdkType' - [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
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


-- | A string-to-string key-value map of query parameters @sdkType@ -dependent properties of the SDK. For @sdkType@ of @objectivec@ or @swift@ , a parameter named @classPrefix@ is required. For @sdkType@ of @android@ , parameters named @groupId@ , @artifactId@ , @artifactVersion@ , and @invokerPackage@ are required. For @sdkType@ of @java@ , parameters named @serviceName@ and @javaPackageName@ are required.
gsdkParameters :: Lens' GetSDK (HashMap Text Text)
gsdkParameters = lens _gsdkParameters (\ s a -> s{_gsdkParameters = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
gsdkRestAPIId :: Lens' GetSDK Text
gsdkRestAPIId = lens _gsdkRestAPIId (\ s a -> s{_gsdkRestAPIId = a})

-- | [Required] The name of the 'Stage' that the SDK will use.
gsdkStageName :: Lens' GetSDK Text
gsdkStageName = lens _gsdkStageName (\ s a -> s{_gsdkStageName = a})

-- | [Required] The language for the generated SDK. Currently @java@ , @javascript@ , @android@ , @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are supported.
gsdkSdkType :: Lens' GetSDK Text
gsdkSdkType = lens _gsdkSdkType (\ s a -> s{_gsdkSdkType = a})

instance AWSRequest GetSDK where
        type Rs GetSDK = GetSDKResponse
        request = get apiGateway
        response
          = receiveBytes
              (\ s h x ->
                 GetSDKResponse' <$>
                   (pure (Just x)) <*> (h .#? "Content-Disposition") <*>
                     (h .#? "Content-Type")
                     <*> (pure (fromEnum s)))

instance Hashable GetSDK where

instance NFData GetSDK where

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

-- | The binary blob response to 'GetSdk' , which contains the generated SDK.
--
--
--
-- /See:/ 'getSDKResponse' smart constructor.
data GetSDKResponse = GetSDKResponse'
  { _gsdkrsBody               :: !(Maybe ByteString)
  , _gsdkrsContentDisposition :: !(Maybe Text)
  , _gsdkrsContentType        :: !(Maybe Text)
  , _gsdkrsResponseStatus     :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSDKResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdkrsBody' - The binary blob response to 'GetSdk' , which contains the generated SDK.
--
-- * 'gsdkrsContentDisposition' - The content-disposition header value in the HTTP response.
--
-- * 'gsdkrsContentType' - The content-type header value in the HTTP response.
--
-- * 'gsdkrsResponseStatus' - -- | The response status code.
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


-- | The binary blob response to 'GetSdk' , which contains the generated SDK.
gsdkrsBody :: Lens' GetSDKResponse (Maybe ByteString)
gsdkrsBody = lens _gsdkrsBody (\ s a -> s{_gsdkrsBody = a})

-- | The content-disposition header value in the HTTP response.
gsdkrsContentDisposition :: Lens' GetSDKResponse (Maybe Text)
gsdkrsContentDisposition = lens _gsdkrsContentDisposition (\ s a -> s{_gsdkrsContentDisposition = a})

-- | The content-type header value in the HTTP response.
gsdkrsContentType :: Lens' GetSDKResponse (Maybe Text)
gsdkrsContentType = lens _gsdkrsContentType (\ s a -> s{_gsdkrsContentType = a})

-- | -- | The response status code.
gsdkrsResponseStatus :: Lens' GetSDKResponse Int
gsdkrsResponseStatus = lens _gsdkrsResponseStatus (\ s a -> s{_gsdkrsResponseStatus = a})

instance NFData GetSDKResponse where
