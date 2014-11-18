{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.SetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the necessary configuration for push sync.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_SetIdentityPoolConfiguration.html>
module Network.AWS.CognitoSync.SetIdentityPoolConfiguration
    (
    -- * Request
      SetIdentityPoolConfiguration
    -- ** Request constructor
    , setIdentityPoolConfiguration
    -- ** Request lenses
    , sipcIdentityPoolId
    , sipcPushSync

    -- * Response
    , SetIdentityPoolConfigurationResponse
    -- ** Response constructor
    , setIdentityPoolConfigurationResponse
    -- ** Response lenses
    , sipcrIdentityPoolId
    , sipcrPushSync
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration
    { _sipcIdentityPoolId :: Text
    , _sipcPushSync       :: Maybe PushSync
    } deriving (Eq, Show, Generic)

-- | 'SetIdentityPoolConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sipcIdentityPoolId' @::@ 'Text'
--
-- * 'sipcPushSync' @::@ 'Maybe' 'PushSync'
--
setIdentityPoolConfiguration :: Text -- ^ 'sipcIdentityPoolId'
                             -> SetIdentityPoolConfiguration
setIdentityPoolConfiguration p1 = SetIdentityPoolConfiguration
    { _sipcIdentityPoolId = p1
    , _sipcPushSync       = Nothing
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool to modify.
sipcIdentityPoolId :: Lens' SetIdentityPoolConfiguration Text
sipcIdentityPoolId =
    lens _sipcIdentityPoolId (\s a -> s { _sipcIdentityPoolId = a })

-- | Configuration options to be applied to the identity pool.
sipcPushSync :: Lens' SetIdentityPoolConfiguration (Maybe PushSync)
sipcPushSync = lens _sipcPushSync (\s a -> s { _sipcPushSync = a })

data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse
    { _sipcrIdentityPoolId :: Maybe Text
    , _sipcrPushSync       :: Maybe PushSync
    } deriving (Eq, Show, Generic)

-- | 'SetIdentityPoolConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sipcrIdentityPoolId' @::@ 'Maybe' 'Text'
--
-- * 'sipcrPushSync' @::@ 'Maybe' 'PushSync'
--
setIdentityPoolConfigurationResponse :: SetIdentityPoolConfigurationResponse
setIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse
    { _sipcrIdentityPoolId = Nothing
    , _sipcrPushSync       = Nothing
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
sipcrIdentityPoolId :: Lens' SetIdentityPoolConfigurationResponse (Maybe Text)
sipcrIdentityPoolId =
    lens _sipcrIdentityPoolId (\s a -> s { _sipcrIdentityPoolId = a })

-- | Configuration options applied to the identity pool.
sipcrPushSync :: Lens' SetIdentityPoolConfigurationResponse (Maybe PushSync)
sipcrPushSync = lens _sipcrPushSync (\s a -> s { _sipcrPushSync = a })

instance ToPath SetIdentityPoolConfiguration where
    toPath SetIdentityPoolConfiguration{..} = mconcat
        [ "/identitypools/"
        , toText _sipcIdentityPoolId
        , "/configuration"
        ]

instance ToQuery SetIdentityPoolConfiguration where
    toQuery = const mempty

instance ToHeaders SetIdentityPoolConfiguration

instance ToJSON SetIdentityPoolConfiguration where
    toJSON SetIdentityPoolConfiguration{..} = object
        [ "PushSync" .= _sipcPushSync
        ]

instance AWSRequest SetIdentityPoolConfiguration where
    type Sv SetIdentityPoolConfiguration = CognitoSync
    type Rs SetIdentityPoolConfiguration = SetIdentityPoolConfigurationResponse

    request  = post
    response = jsonResponse

instance FromJSON SetIdentityPoolConfigurationResponse where
    parseJSON = withObject "SetIdentityPoolConfigurationResponse" $ \o -> SetIdentityPoolConfigurationResponse
        <$> o .:? "IdentityPoolId"
        <*> o .:? "PushSync"
