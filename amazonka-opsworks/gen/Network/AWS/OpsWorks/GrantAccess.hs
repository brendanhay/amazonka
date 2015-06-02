{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.GrantAccess
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This API can be used only with Windows stacks. Grants RDP access to a Windows
-- instance for a specified time period.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_GrantAccess.html>
module Network.AWS.OpsWorks.GrantAccess
    (
    -- * Request
      GrantAccess
    -- ** Request constructor
    , grantAccess
    -- ** Request lenses
    , gaInstanceId
    , gaValidForInMinutes

    -- * Response
    , GrantAccessResponse
    -- ** Response constructor
    , grantAccessResponse
    -- ** Response lenses
    , garTemporaryCredential
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data GrantAccess = GrantAccess
    { _gaInstanceId        :: Text
    , _gaValidForInMinutes :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'GrantAccess' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaInstanceId' @::@ 'Text'
--
-- * 'gaValidForInMinutes' @::@ 'Maybe' 'Natural'
--
grantAccess :: Text -- ^ 'gaInstanceId'
            -> GrantAccess
grantAccess p1 = GrantAccess
    { _gaInstanceId        = p1
    , _gaValidForInMinutes = Nothing
    }

-- | The instance's AWS OpsWorks ID.
gaInstanceId :: Lens' GrantAccess Text
gaInstanceId = lens _gaInstanceId (\s a -> s { _gaInstanceId = a })

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires at the end of this period, the user will no longer be able to use the
-- credentials to log in. If the user is logged in at the time, he or she
-- automatically will be logged out.
gaValidForInMinutes :: Lens' GrantAccess (Maybe Natural)
gaValidForInMinutes =
    lens _gaValidForInMinutes (\s a -> s { _gaValidForInMinutes = a })
        . mapping _Nat

newtype GrantAccessResponse = GrantAccessResponse
    { _garTemporaryCredential :: Maybe TemporaryCredential
    } deriving (Eq, Read, Show)

-- | 'GrantAccessResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garTemporaryCredential' @::@ 'Maybe' 'TemporaryCredential'
--
grantAccessResponse :: GrantAccessResponse
grantAccessResponse = GrantAccessResponse
    { _garTemporaryCredential = Nothing
    }

-- | A 'TemporaryCredential' object that contains the data needed to log in to the
-- instance by RDP clients, such as the Microsoft Remote Desktop Connection.
garTemporaryCredential :: Lens' GrantAccessResponse (Maybe TemporaryCredential)
garTemporaryCredential =
    lens _garTemporaryCredential (\s a -> s { _garTemporaryCredential = a })

instance ToPath GrantAccess where
    toPath = const "/"

instance ToQuery GrantAccess where
    toQuery = const mempty

instance ToHeaders GrantAccess

instance ToJSON GrantAccess where
    toJSON GrantAccess{..} = object
        [ "InstanceId"        .= _gaInstanceId
        , "ValidForInMinutes" .= _gaValidForInMinutes
        ]

instance AWSRequest GrantAccess where
    type Sv GrantAccess = OpsWorks
    type Rs GrantAccess = GrantAccessResponse

    request  = post "GrantAccess"
    response = jsonResponse

instance FromJSON GrantAccessResponse where
    parseJSON = withObject "GrantAccessResponse" $ \o -> GrantAccessResponse
        <$> o .:? "TemporaryCredential"
