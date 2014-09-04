{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified HSM client certificate.
module Network.AWS.Redshift.V2012_12_01.DeleteHsmClientCertificate
    (
    -- * Request
      DeleteHsmClientCertificate
    -- ** Request constructor
    , mkDeleteHsmClientCertificateMessage
    -- ** Request lenses
    , dhccmHsmClientCertificateIdentifier

    -- * Response
    , DeleteHsmClientCertificateResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteHsmClientCertificate' request.
mkDeleteHsmClientCertificateMessage :: Text -- ^ 'dhccmHsmClientCertificateIdentifier'
                                    -> DeleteHsmClientCertificate
mkDeleteHsmClientCertificateMessage p1 = DeleteHsmClientCertificate
    { _dhccmHsmClientCertificateIdentifier = p1
    }
{-# INLINE mkDeleteHsmClientCertificateMessage #-}

newtype DeleteHsmClientCertificate = DeleteHsmClientCertificate
    { _dhccmHsmClientCertificateIdentifier :: Text
      -- ^ The identifier of the HSM client certificate to be deleted.
    } deriving (Show, Generic)

-- | The identifier of the HSM client certificate to be deleted.
dhccmHsmClientCertificateIdentifier :: Lens' DeleteHsmClientCertificate (Text)
dhccmHsmClientCertificateIdentifier = lens _dhccmHsmClientCertificateIdentifier (\s a -> s { _dhccmHsmClientCertificateIdentifier = a })
{-# INLINE dhccmHsmClientCertificateIdentifier #-}

instance ToQuery DeleteHsmClientCertificate where
    toQuery = genericQuery def

data DeleteHsmClientCertificateResponse = DeleteHsmClientCertificateResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteHsmClientCertificate where
    type Sv DeleteHsmClientCertificate = Redshift
    type Rs DeleteHsmClientCertificate = DeleteHsmClientCertificateResponse

    request = post "DeleteHsmClientCertificate"
    response _ = nullaryResponse DeleteHsmClientCertificateResponse
