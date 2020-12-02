{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.Product where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types.Sum
import Network.AWS.Prelude

-- | A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.
--
--
--
-- /See:/ 'corsRule' smart constructor.
data CORSRule = CORSRule'
  { _crAllowedMethods :: !(Maybe [MethodName])
  , _crMaxAgeSeconds  :: !(Maybe Nat)
  , _crAllowedHeaders :: !(Maybe [Text])
  , _crAllowedOrigins :: !(Maybe [Text])
  , _crExposeHeaders  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CORSRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crAllowedMethods' - Identifies an HTTP method that the origin that is specified in the rule is allowed to execute. Each CORS rule must contain at least one @AllowedMethod@ and one @AllowedOrigin@ element.
--
-- * 'crMaxAgeSeconds' - The time in seconds that your browser caches the preflight response for the specified resource. A CORS rule can have only one @MaxAgeSeconds@ element.
--
-- * 'crAllowedHeaders' - Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back.  This element can contain only one wildcard character (*).
--
-- * 'crAllowedOrigins' - One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object). Each CORS rule must have at least one @AllowedOrigin@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
--
-- * 'crExposeHeaders' - One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object). This element is optional for each rule.
corsRule
    :: CORSRule
corsRule =
  CORSRule'
    { _crAllowedMethods = Nothing
    , _crMaxAgeSeconds = Nothing
    , _crAllowedHeaders = Nothing
    , _crAllowedOrigins = Nothing
    , _crExposeHeaders = Nothing
    }


-- | Identifies an HTTP method that the origin that is specified in the rule is allowed to execute. Each CORS rule must contain at least one @AllowedMethod@ and one @AllowedOrigin@ element.
crAllowedMethods :: Lens' CORSRule [MethodName]
crAllowedMethods = lens _crAllowedMethods (\ s a -> s{_crAllowedMethods = a}) . _Default . _Coerce

-- | The time in seconds that your browser caches the preflight response for the specified resource. A CORS rule can have only one @MaxAgeSeconds@ element.
crMaxAgeSeconds :: Lens' CORSRule (Maybe Natural)
crMaxAgeSeconds = lens _crMaxAgeSeconds (\ s a -> s{_crMaxAgeSeconds = a}) . mapping _Nat

-- | Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back.  This element can contain only one wildcard character (*).
crAllowedHeaders :: Lens' CORSRule [Text]
crAllowedHeaders = lens _crAllowedHeaders (\ s a -> s{_crAllowedHeaders = a}) . _Default . _Coerce

-- | One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object). Each CORS rule must have at least one @AllowedOrigin@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
crAllowedOrigins :: Lens' CORSRule [Text]
crAllowedOrigins = lens _crAllowedOrigins (\ s a -> s{_crAllowedOrigins = a}) . _Default . _Coerce

-- | One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object). This element is optional for each rule.
crExposeHeaders :: Lens' CORSRule [Text]
crExposeHeaders = lens _crExposeHeaders (\ s a -> s{_crExposeHeaders = a}) . _Default . _Coerce

instance FromJSON CORSRule where
        parseJSON
          = withObject "CORSRule"
              (\ x ->
                 CORSRule' <$>
                   (x .:? "AllowedMethods" .!= mempty) <*>
                     (x .:? "MaxAgeSeconds")
                     <*> (x .:? "AllowedHeaders" .!= mempty)
                     <*> (x .:? "AllowedOrigins" .!= mempty)
                     <*> (x .:? "ExposeHeaders" .!= mempty))

instance Hashable CORSRule where

instance NFData CORSRule where

instance ToJSON CORSRule where
        toJSON CORSRule'{..}
          = object
              (catMaybes
                 [("AllowedMethods" .=) <$> _crAllowedMethods,
                  ("MaxAgeSeconds" .=) <$> _crMaxAgeSeconds,
                  ("AllowedHeaders" .=) <$> _crAllowedHeaders,
                  ("AllowedOrigins" .=) <$> _crAllowedOrigins,
                  ("ExposeHeaders" .=) <$> _crExposeHeaders])

-- | This section describes operations that you can perform on an AWS Elemental MediaStore container.
--
--
--
-- /See:/ 'container' smart constructor.
data Container = Container'
  { _cCreationTime :: !(Maybe POSIX)
  , _cStatus       :: !(Maybe ContainerStatus)
  , _cARN          :: !(Maybe Text)
  , _cName         :: !(Maybe Text)
  , _cEndpoint     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - Unix timestamp.
--
-- * 'cStatus' - The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
--
-- * 'cARN' - The Amazon Resource Name (ARN) of the container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>  For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- * 'cName' - The name of the container.
--
-- * 'cEndpoint' - The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
container
    :: Container
container =
  Container'
    { _cCreationTime = Nothing
    , _cStatus = Nothing
    , _cARN = Nothing
    , _cName = Nothing
    , _cEndpoint = Nothing
    }


-- | Unix timestamp.
cCreationTime :: Lens' Container (Maybe UTCTime)
cCreationTime = lens _cCreationTime (\ s a -> s{_cCreationTime = a}) . mapping _Time

-- | The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
cStatus :: Lens' Container (Maybe ContainerStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The Amazon Resource Name (ARN) of the container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>  For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
cARN :: Lens' Container (Maybe Text)
cARN = lens _cARN (\ s a -> s{_cARN = a})

-- | The name of the container.
cName :: Lens' Container (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
cEndpoint :: Lens' Container (Maybe Text)
cEndpoint = lens _cEndpoint (\ s a -> s{_cEndpoint = a})

instance FromJSON Container where
        parseJSON
          = withObject "Container"
              (\ x ->
                 Container' <$>
                   (x .:? "CreationTime") <*> (x .:? "Status") <*>
                     (x .:? "ARN")
                     <*> (x .:? "Name")
                     <*> (x .:? "Endpoint"))

instance Hashable Container where

instance NFData Container where
