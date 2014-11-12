{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Redshift.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified HSM client certificate.
module Network.AWS.Redshift.DeleteHsmClientCertificate
    (
    -- * Request
      DeleteHsmClientCertificateMessage
    -- ** Request constructor
    , deleteHsmClientCertificate
    -- ** Request lenses
    , dhccm1HsmClientCertificateIdentifier

    -- * Response
    , DeleteHsmClientCertificateResponse
    -- ** Response constructor
    , deleteHsmClientCertificateResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DeleteHsmClientCertificateMessage = DeleteHsmClientCertificateMessage
    { _dhccm1HsmClientCertificateIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteHsmClientCertificateMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhccm1HsmClientCertificateIdentifier' @::@ 'Text'
--
deleteHsmClientCertificate :: Text -- ^ 'dhccm1HsmClientCertificateIdentifier'
                           -> DeleteHsmClientCertificateMessage
deleteHsmClientCertificate p1 = DeleteHsmClientCertificateMessage
    { _dhccm1HsmClientCertificateIdentifier = p1
    }

-- | The identifier of the HSM client certificate to be deleted.
dhccm1HsmClientCertificateIdentifier :: Lens' DeleteHsmClientCertificateMessage Text
dhccm1HsmClientCertificateIdentifier =
    lens _dhccm1HsmClientCertificateIdentifier
        (\s a -> s { _dhccm1HsmClientCertificateIdentifier = a })

instance ToQuery DeleteHsmClientCertificateMessage

instance ToPath DeleteHsmClientCertificateMessage where
    toPath = const "/"

data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteHsmClientCertificateResponse' constructor.
deleteHsmClientCertificateResponse :: DeleteHsmClientCertificateResponse
deleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse

instance FromXML DeleteHsmClientCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteHsmClientCertificateResponse"

instance AWSRequest DeleteHsmClientCertificateMessage where
    type Sv DeleteHsmClientCertificateMessage = Redshift
    type Rs DeleteHsmClientCertificateMessage = DeleteHsmClientCertificateResponse

    request  = post "DeleteHsmClientCertificate"
    response = nullaryResponse DeleteHsmClientCertificateResponse
