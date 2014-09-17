{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Updates the name and/or the path of the specified server certificate. You
-- should understand the implications of changing a server certificate's path
-- or name. For more information, see Managing Server Certificates in the
-- Using IAM guide. To change a server certificate name the requester must
-- have appropriate permissions on both the source object and the target
-- object. For example, to change the name from ProductionCert to ProdCert,
-- the entity making the request must have permission on ProductionCert and
-- ProdCert, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateServerCertificate &ServerCertificateName=ProdServerCert
-- &NewServerCertificateName=ProdServerCertName &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateServerCertificate
    (
    -- * Request
      UpdateServerCertificate
    -- ** Request constructor
    , mkUpdateServerCertificate
    -- ** Request lenses
    , uscServerCertificateName
    , uscNewPath
    , uscNewServerCertificateName

    -- * Response
    , UpdateServerCertificateResponse
    -- ** Response constructor
    , mkUpdateServerCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateServerCertificate = UpdateServerCertificate
    { _uscServerCertificateName :: Text
    , _uscNewPath :: Maybe Text
    , _uscNewServerCertificateName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateServerCertificate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServerCertificateName ::@ @Text@
--
-- * @NewPath ::@ @Maybe Text@
--
-- * @NewServerCertificateName ::@ @Maybe Text@
--
mkUpdateServerCertificate :: Text -- ^ 'uscServerCertificateName'
                          -> UpdateServerCertificate
mkUpdateServerCertificate p1 = UpdateServerCertificate
    { _uscServerCertificateName = p1
    , _uscNewPath = Nothing
    , _uscNewServerCertificateName = Nothing
    }

-- | The name of the server certificate that you want to update.
uscServerCertificateName :: Lens' UpdateServerCertificate Text
uscServerCertificateName =
    lens _uscServerCertificateName
         (\s a -> s { _uscServerCertificateName = a })

-- | The new path for the server certificate. Include this only if you are
-- updating the server certificate's path.
uscNewPath :: Lens' UpdateServerCertificate (Maybe Text)
uscNewPath = lens _uscNewPath (\s a -> s { _uscNewPath = a })

-- | The new name for the server certificate. Include this only if you are
-- updating the server certificate's name.
uscNewServerCertificateName :: Lens' UpdateServerCertificate (Maybe Text)
uscNewServerCertificateName =
    lens _uscNewServerCertificateName
         (\s a -> s { _uscNewServerCertificateName = a })

instance ToQuery UpdateServerCertificate where
    toQuery = genericQuery def

data UpdateServerCertificateResponse = UpdateServerCertificateResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateServerCertificateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateServerCertificateResponse :: UpdateServerCertificateResponse
mkUpdateServerCertificateResponse = UpdateServerCertificateResponse

instance AWSRequest UpdateServerCertificate where
    type Sv UpdateServerCertificate = IAM
    type Rs UpdateServerCertificate = UpdateServerCertificateResponse

    request = post "UpdateServerCertificate"
    response _ = nullaryResponse UpdateServerCertificateResponse
