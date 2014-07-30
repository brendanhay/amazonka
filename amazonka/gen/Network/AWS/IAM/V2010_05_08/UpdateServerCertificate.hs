{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateServerCertificate
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
module Network.AWS.IAM.V2010_05_08.UpdateServerCertificate where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateServerCertificate' request.
updateServerCertificate :: Text -- ^ '_uscwServerCertificateName'
                        -> UpdateServerCertificate
updateServerCertificate p1 = UpdateServerCertificate
    { _uscwServerCertificateName = p1
    , _uscwNewPath = Nothing
    , _uscwNewServerCertificateName = Nothing
    }

data UpdateServerCertificate = UpdateServerCertificate
    { _uscwServerCertificateName :: Text
      -- ^ The name of the server certificate that you want to update.
    , _uscwNewPath :: Maybe Text
      -- ^ The new path for the server certificate. Include this only if you
      -- are updating the server certificate's path.
    , _uscwNewServerCertificateName :: Maybe Text
      -- ^ The new name for the server certificate. Include this only if you
      -- are updating the server certificate's name.
    } deriving (Generic)

instance ToQuery UpdateServerCertificate where
    toQuery = genericToQuery def

instance AWSRequest UpdateServerCertificate where
    type Sv UpdateServerCertificate = IAM
    type Rs UpdateServerCertificate = UpdateServerCertificateResponse

    request = post "UpdateServerCertificate"
    response _ _ = return (Right UpdateServerCertificateResponse)

data UpdateServerCertificateResponse = UpdateServerCertificateResponse
    deriving (Eq, Show, Generic)
