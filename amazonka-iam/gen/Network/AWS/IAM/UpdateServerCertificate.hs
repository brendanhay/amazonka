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

-- Module      : Network.AWS.IAM.UpdateServerCertificate
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

-- | Updates the name and/or the path of the specified server certificate.
--
-- You should understand the implications of changing a server certificate's
-- path or name. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingServerCerts.html Managing Server Certificates> in the /Using IAM/ guide.   To change a server certificate name the requester must have
-- appropriate permissions on both the source object and the target object. For
-- example, to change the name from ProductionCert to ProdCert, the entity
-- making the request must have permission on ProductionCert and ProdCert, or
-- must have permission on all (*). For more information about permissions, see Permissions and Policies
-- .
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateServerCertificate.html>
module Network.AWS.IAM.UpdateServerCertificate
    (
    -- * Request
      UpdateServerCertificate
    -- ** Request constructor
    , updateServerCertificate
    -- ** Request lenses
    , usc1NewPath
    , usc1NewServerCertificateName
    , usc1ServerCertificateName

    -- * Response
    , UpdateServerCertificateResponse
    -- ** Response constructor
    , updateServerCertificateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateServerCertificate = UpdateServerCertificate
    { _usc1NewPath                  :: Maybe Text
    , _usc1NewServerCertificateName :: Maybe Text
    , _usc1ServerCertificateName    :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateServerCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usc1NewPath' @::@ 'Maybe' 'Text'
--
-- * 'usc1NewServerCertificateName' @::@ 'Maybe' 'Text'
--
-- * 'usc1ServerCertificateName' @::@ 'Text'
--
updateServerCertificate :: Text -- ^ 'usc1ServerCertificateName'
                        -> UpdateServerCertificate
updateServerCertificate p1 = UpdateServerCertificate
    { _usc1ServerCertificateName    = p1
    , _usc1NewPath                  = Nothing
    , _usc1NewServerCertificateName = Nothing
    }

-- | The new path for the server certificate. Include this only if you are
-- updating the server certificate's path.
usc1NewPath :: Lens' UpdateServerCertificate (Maybe Text)
usc1NewPath = lens _usc1NewPath (\s a -> s { _usc1NewPath = a })

-- | The new name for the server certificate. Include this only if you are
-- updating the server certificate's name.
usc1NewServerCertificateName :: Lens' UpdateServerCertificate (Maybe Text)
usc1NewServerCertificateName =
    lens _usc1NewServerCertificateName
        (\s a -> s { _usc1NewServerCertificateName = a })

-- | The name of the server certificate that you want to update.
usc1ServerCertificateName :: Lens' UpdateServerCertificate Text
usc1ServerCertificateName =
    lens _usc1ServerCertificateName
        (\s a -> s { _usc1ServerCertificateName = a })

data UpdateServerCertificateResponse = UpdateServerCertificateResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateServerCertificateResponse' constructor.
updateServerCertificateResponse :: UpdateServerCertificateResponse
updateServerCertificateResponse = UpdateServerCertificateResponse

instance ToPath UpdateServerCertificate where
    toPath = const "/"

instance ToQuery UpdateServerCertificate where
    toQuery UpdateServerCertificate{..} = mconcat
        [ "NewPath"                  =? _usc1NewPath
        , "NewServerCertificateName" =? _usc1NewServerCertificateName
        , "ServerCertificateName"    =? _usc1ServerCertificateName
        ]

instance ToHeaders UpdateServerCertificate

instance AWSRequest UpdateServerCertificate where
    type Sv UpdateServerCertificate = IAM
    type Rs UpdateServerCertificate = UpdateServerCertificateResponse

    request  = post "UpdateServerCertificate"
    response = nullResponse UpdateServerCertificateResponse
